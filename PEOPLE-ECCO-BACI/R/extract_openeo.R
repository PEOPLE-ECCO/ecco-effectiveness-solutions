# =============================================================================
# extract_openeo.R
#
# Standalone function for extracting covariate values from openEO backends
# using server-side aggregate_spatial, avoiding raster downloads entirely.
#
# Usage inside the Shiny app: sourced automatically via global.R.
# Usage outside the app:
#   source("R/extract_openeo.R")
#   library(openeo)
#   library(terra)
#
# Main entry point:
#   extract_openeo(y, backend, collection, bands, spatial_reducer, col_uid)
# =============================================================================


# Pre-defined collections available in this demonstrator.
# Each entry: list(label, id, bands, description)
OPENEO_COLLECTIONS <- list(
  list(
    id          = "COPERNICUS_30",
    label       = "Copernicus DEM 30m (COPERNICUS_30)",
    bands       = c("DEM"),
    description = "Copernicus Digital Elevation Model at 30m resolution.",
    temporal    = FALSE
  ),
  list(
    id          = "ESA_WORLDCOVER_10M_2020_V1",
    label       = "ESA WorldCover 2020 10m (ESA_WORLDCOVER_10M_2020_V1)",
    bands       = c("MAP"),
    description = "ESA WorldCover land cover map for 2020 at 10m resolution.",
    temporal    = FALSE
  ),
  list(
    id          = "ESA_WORLDCOVER_10M_2021_V2",
    label       = "ESA WorldCover 2021 10m (ESA_WORLDCOVER_10M_2021_V2)",
    bands       = c("MAP"),
    description = "ESA WorldCover land cover map for 2021 at 10m resolution.",
    temporal    = FALSE
  )
)

# Named vector for selectInput choices: label -> id
openeo_collection_choices <- function() {
  ids    <- vapply(OPENEO_COLLECTIONS, `[[`, character(1), "id")
  labels <- vapply(OPENEO_COLLECTIONS, `[[`, character(1), "label")
  setNames(ids, labels)
}

# Band choices for a given collection id
openeo_band_choices <- function(collection_id) {
  for (coll in OPENEO_COLLECTIONS) {
    if (coll$id == collection_id) return(coll$bands)
  }
  character(0)
}


# -----------------------------------------------------------------------------
# openeo_connect()
#
# Connects to an openEO backend and authenticates interactively.
# Returns the connection object, or stops with an informative error.
#
# Arguments:
#   backend_url   Character. URL of the openEO backend.
#                 Default: CDSE ("https://openeo.dataspace.copernicus.eu")
# -----------------------------------------------------------------------------
openeo_connect <- function(
    backend_url = "https://openeo.dataspace.copernicus.eu") {

  if (!requireNamespace("openeo", quietly = TRUE)) {
    stop("Package 'openeo' is required. Install with: install.packages('openeo')")
  }

  con <- tryCatch(
    openeo::connect(backend_url),
    error = function(e) {
      stop("Could not connect to openEO backend '", backend_url,
           "': ", conditionMessage(e))
    }
  )

  # Interactive OAuth2 login
  tryCatch(
    openeo::login(),
    error = function(e) {
      stop("Authentication failed: ", conditionMessage(e))
    }
  )

  message("Connected and authenticated to: ", backend_url)
  con
}


# -----------------------------------------------------------------------------
# extract_openeo()
#
# Extracts covariate values from an openEO collection for all features in a
# SpatVector using server-side aggregate_spatial. No raster is downloaded;
# only the aggregated values per feature are returned.
#
# Arguments:
#   y                SpatVector. Input features (polygons or points).
#   collection       Character. openEO collection ID, e.g. "COPERNICUS_DEM".
#   bands            Character vector. Band names to extract.
#   spatial_reducer  Character. openEO process name for spatial aggregation:
#                    "mean", "median", "min", "max", "sum", "sd".
#                    Default "mean".
#   col_uid          Character. Name of the unique ID column in y, used to
#                    align the result rows with the original features.
#   layer_prefix     Character. Prefix for output column names.
#                    Default: collection ID.
#   con              openEO connection object (from openeo_connect()).
#                    If NULL, openeo_connect() is called automatically.
#   backend_url      Character. Used only when con is NULL.
#
# Returns a data.frame with nrow(y) rows and one column per band,
# named <layer_prefix>_<band>.
# -----------------------------------------------------------------------------
extract_openeo <- function(y,
                           collection,
                           bands           = NULL,
                           spatial_reducer = "mean",
                           col_uid         = NULL,
                           layer_prefix    = NULL,
                           con             = NULL,
                           backend_url     = "https://openeo.dataspace.copernicus.eu",
                           progress_fun    = NULL) {

  notify <- function(msg) {
    message(msg)
    if (!is.null(progress_fun)) { progress_fun(msg) }
  }

  if (!requireNamespace("openeo", quietly = TRUE)) {
    stop("Package 'openeo' is required.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.")
  }

  # -- Connect if needed ------------------------------------------------------
  if (is.null(con)) {
    con <- openeo_connect(backend_url)
  }

  # -- Default bands from pre-defined collection list -------------------------
  if (is.null(bands) || length(bands) == 0) {
    bands <- openeo_band_choices(collection)
    if (length(bands) == 0) bands <- NULL   # let backend decide
  }

  # -- Column prefix ----------------------------------------------------------
  if (is.null(layer_prefix) || nchar(layer_prefix) == 0) {
    layer_prefix <- make_colname(collection)
  }

  # -- Reproject y to WGS84 for openEO (always expects EPSG:4326) ------------
  # GeoJSON format mandates EPSG:4326; always reproject regardless of input CRS
  # to avoid writing UTM coordinates with an incorrect lon/lat CRS label.
  # suppressWarnings: terra::is.lonlat warns on ambiguous CRS - we handle it.
  is_lonlat <- suppressWarnings(terra::is.lonlat(y))
  y_wgs <- if (!isTRUE(is_lonlat)) {
    terra::project(y, "EPSG:4326")
  } else {
    y
  }

  # -- Spatial extent (bounding box) -----------------------------------------
  # CDSE expects a plain named list with west/east/south/north as plain
  # numeric scalars (not SpatExtent objects).
  ext   <- as.vector(terra::ext(y_wgs))  # [xmin, xmax, ymin, ymax]
  bbox  <- list(
    west  = unname(ext[1]),
    east  = unname(ext[2]),
    south = unname(ext[3]),
    north = unname(ext[4])
  )

  # -- Export geometries as GeoJSON FeatureCollection -------------------------
  # Strip all attributes before export (geometry only) to minimise payload.
  y_geom          <- y_wgs
  y_geom$.tmp_uid <- as.character(seq_len(nrow(y_geom)))
  y_geom          <- y_geom[, ".tmp_uid"]

  tmp_geojson <- tempfile(fileext = ".geojson")
  terra::writeVector(y_geom, tmp_geojson, filetype = "GeoJSON", overwrite = TRUE)
  geom_json <- jsonlite::read_json(tmp_geojson)

  # -- Build process graph ----------------------------------------------------
  p <- openeo::processes()

  # bands must be a non-empty list or omitted entirely (NULL not accepted by CDSE)
  band_arg <- if (!is.null(bands) && length(bands) > 0) {
    as.list(bands)
  } else {
    NULL
  }

  cube <- p$load_collection(
    id             = collection,
    spatial_extent = bbox,
    bands          = band_arg
  )

  # Reducer: must return a single value per pixel stack.
  # Use process shortcuts rather than anonymous functions for CDSE compatibility.
  reducer_fun <- switch(spatial_reducer,
    mean   = function(data, context) { p$mean(data) },
    median = function(data, context) { p$median(data) },
    min    = function(data, context) { p$min(data) },
    max    = function(data, context) { p$max(data) },
    sum    = function(data, context) { p$sum(data) },
    sd     = function(data, context) { p$sd(data) },
    stop("Unknown spatial_reducer: '", spatial_reducer,
         "'. Use: mean, median, min, max, sum, sd.")
  )

  # aggregate_spatial: server computes one value per feature per band.
  # Pass geometries as the parsed FeatureCollection list.
  aggregated <- p$aggregate_spatial(
    data       = cube,
    geometries = geom_json,
    reducer    = reducer_fun
  )

  # Try JSON format first (correct for aggregate_spatial tabular output).
  # Fall back to netCDF if CDSE rejects JSON.
  # tmp_result and state variables are defined before the loop so they are
  # visible inside tryCatch error handlers (R closure scoping).
  tmp_result <- tempfile(fileext = ".json")
  notify(paste0("openEO: submitting process graph to ", backend_url, "..."))

  compute_ok <- FALSE
  last_err   <- "unknown error"

  for (fmt in c("JSON", "netCDF")) {
    result_proc_fmt <- p$save_result(aggregated, format = fmt)
    out_file <- if (fmt == "JSON") {
      tmp_result
    } else {
      tempfile(fileext = ".nc")
    }
    ok <- tryCatch({
      openeo::compute_result(result_proc_fmt, output_file = out_file)
      TRUE
    }, error = function(e) {
      last_err <<- conditionMessage(e)
      notify(paste0("openEO: format '", fmt, "' error: ",
                    substr(last_err, 1, 120)))
      FALSE
    })
    if (isTRUE(ok)) {
      compute_ok <- TRUE
      tmp_result <- out_file   # update in case netCDF was used
      notify(paste0("openEO: result received (format: ", fmt, "), parsing..."))
      break
    }
  }
  if (!compute_ok) {
    stop("openEO compute_result() failed. Last error: ", last_err)
  }

  # -- Parse result ----------------------------------------------------------
  # Read without simplifyVector to preserve the exact nested structure.
  # CDSE aggregate_spatial returns JSON in the format:
  #   [[band_value, ...], [band_value, ...], ...]   (one array per feature)
  # simplifyVector=TRUE can collapse this incorrectly for multi-band results.
  raw <- jsonlite::read_json(tmp_result, simplifyVector = FALSE)

  result_df <- parse_aggregate_spatial_result(raw, bands, nrow(y),
                                              time_reducer = spatial_reducer)

  if (nrow(result_df) != nrow(y)) {
    warning("openEO: expected ", nrow(y), " rows but got ", nrow(result_df),
            ". Results may be misaligned.")
  }

  # -- Name columns ----------------------------------------------------------
  if (ncol(result_df) == length(bands)) {
    names(result_df) <- paste0(layer_prefix, "_",
                               vapply(bands, make_colname, character(1)))
  } else {
    names(result_df) <- paste0(layer_prefix, "_",
                               seq_len(ncol(result_df)))
  }

  # -- Align rows to original y via col_uid --------------------------------
  # Result rows are in the same order as the GeoJSON features (terra preserves
  # row order), so alignment is positional. Add the user-supplied UID column
  # if provided, so the result can be merged back by key downstream.
  # If no col_uid was supplied, the caller will join by row position.
  if (!is.null(col_uid) && nchar(col_uid) > 0 && col_uid %in% names(y)) {
    result_df[[col_uid]] <- as.character(as.data.frame(y)[[col_uid]])
  }

  result_df
}


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# parse_aggregate_spatial_result()
#
# Parses the JSON output of CDSE aggregate_spatial into a data.frame with
# one row per feature and one column per band.
#
# CDSE returns a 3-level nested list: [[timestamp]][[feature]][[band]].
# Even static collections (e.g. COPERNICUS_30) may return multiple timestamps.
# The time dimension is collapsed using time_reducer before returning.
# -----------------------------------------------------------------------------
parse_aggregate_spatial_result <- function(raw, bands, n_features,
                                           time_reducer = "mean") {
  n_bands <- max(length(bands), 1L)

  # Helper: extract one numeric value per band from a single entry
  extract_vals <- function(entry) {
    vals <- tryCatch(as.numeric(unlist(entry, use.names = FALSE)),
                     error = function(e) rep(NA_real_, n_bands))
    if (length(vals) == 0) { return(rep(NA_real_, n_bands)) }
    length(vals) <- n_bands
    vals
  }

  # Helper: reduce a list of per-timestamp vectors for one feature
  reduce_time <- function(time_list, reducer) {
    mat <- do.call(rbind, lapply(time_list, extract_vals))
    apply(mat, 2, switch(reducer,
      mean   = function(x) mean(x,   na.rm = TRUE),
      median = function(x) median(x, na.rm = TRUE),
      min    = function(x) min(x,    na.rm = TRUE),
      max    = function(x) max(x,    na.rm = TRUE),
      sum    = function(x) sum(x,    na.rm = TRUE),
      sd     = function(x) sd(x,     na.rm = TRUE),
      function(x) mean(x, na.rm = TRUE)
    ))
  }

  # Case A: already a data.frame
  if (is.data.frame(raw)) {
    return(as.data.frame(lapply(raw, as.numeric)))
  }

  if (!is.list(raw)) {
    warning("Unexpected raw type: ", class(raw))
    return(as.data.frame(matrix(NA_real_, nrow = n_features, ncol = n_bands)))
  }

  n_top <- length(raw)

  # Case B: [[timestamp]][[feature]][[band]] -- confirmed CDSE structure.
  # Outer length != n_features but inner length == n_features.
  if (n_top != n_features && is.list(raw[[1]]) &&
      length(raw[[1]]) == n_features) {
    rows <- lapply(seq_len(n_features), function(fi) {
      time_vals <- lapply(raw, function(ts) ts[[fi]])
      reduce_time(time_vals, time_reducer)
    })
    out <- as.data.frame(do.call(rbind, rows))
    rownames(out) <- NULL
    return(out)
  }

  # Case C: [[feature]][[band]] -- flat, no time dimension
  if (n_top == n_features) {
    rows <- lapply(raw, function(feat) {
      inner <- if (is.list(feat) && length(feat) == 1 &&
                   is.list(feat[[1]])) { feat[[1]] } else { feat }
      extract_vals(inner)
    })
    out <- as.data.frame(do.call(rbind, rows))
    rownames(out) <- NULL
    return(out)
  }

  # Case D: flat numeric, one value per feature
  flat <- suppressWarnings(as.numeric(unlist(raw, use.names = FALSE)))
  if (length(flat) == n_features) {
    return(data.frame(V1 = flat))
  }

  # Case E: flat length == n_features * n_bands
  if (length(flat) == n_features * n_bands) {
    out <- as.data.frame(matrix(flat, nrow = n_features,
                                ncol = n_bands, byrow = TRUE))
    return(out)
  }

  warning("Could not parse openEO aggregate_spatial result. ",
          "Got: ", class(raw), " length=", n_top,
          ". Expected ", n_features, " x ", n_bands, ". Returning NAs.")
  as.data.frame(matrix(NA_real_, nrow = n_features, ncol = n_bands))
}


# -----------------------------------------------------------------------------
# extract_openeo_dem()
#
# Dedicated function for extracting terrain parameters from the Copernicus
# 30m DEM on CDSE using openEO. Supports elevation, slope, aspect, and the
# trigonometric aspect components northness (cos) and eastness (sin).
#
# Arguments:
#   y                SpatVector. Input features (polygons or points).
#   terrain_params   Character vector. Any of: "elevation", "slope", "aspect",
#                    "northness", "eastness". "northness"/"eastness" imply aspect.
#   spatial_reducer  Character. openEO process for spatial aggregation:
#                    "mean", "median", "min", "max", "sum", "sd". Default "mean".
#   col_uid          Character. UID column in y for row alignment.
#   con              openEO connection object or NULL (triggers openeo_connect).
#   backend_url      Character. CDSE URL.
#   progress_fun     Function(msg) for Shiny notifications.
#
# Returns a data.frame with nrow(y) rows and one column per requested parameter.
# -----------------------------------------------------------------------------
extract_openeo_dem <- function(y,
                               terrain_params  = "elevation",
                               spatial_reducer = "mean",
                               col_uid         = NULL,
                               con             = NULL,
                               backend_url     = "https://openeo.dataspace.copernicus.eu",
                               progress_fun    = NULL) {

  notify <- function(msg) {
    message(msg)
    if (!is.null(progress_fun)) { progress_fun(msg) }
  }

  if (!requireNamespace("openeo",   quietly = TRUE)) stop("Package 'openeo' required.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' required.")

  if (is.null(con)) { con <- openeo_connect(backend_url) }

  # Normalise requested parameters
  terrain_params <- unique(tolower(terrain_params))
  need_aspect    <- any(terrain_params %in% c("aspect", "northness", "eastness"))
  need_slope     <- "slope" %in% terrain_params
  need_elevation <- "elevation" %in% terrain_params

  # Reproject to WGS84
  is_lonlat <- suppressWarnings(terra::is.lonlat(y))
  y_wgs <- if (!isTRUE(is_lonlat)) terra::project(y, "EPSG:4326") else y

  # Bounding box
  ext  <- as.vector(terra::ext(y_wgs))
  bbox <- list(west  = unname(ext[1]), east  = unname(ext[2]),
               south = unname(ext[3]), north = unname(ext[4]))

  # Export geometry (strip attributes, keep tmp uid for ordering)
  y_geom          <- y_wgs
  y_geom$.tmp_uid <- as.character(seq_len(nrow(y_geom)))
  y_geom          <- y_geom[, ".tmp_uid"]
  tmp_geojson     <- tempfile(fileext = ".geojson")
  terra::writeVector(y_geom, tmp_geojson, filetype = "GeoJSON", overwrite = TRUE)
  geom_json <- jsonlite::read_json(tmp_geojson)
  p <- openeo::processes()

  # Load DEM collection
  cube <- p$load_collection(
    id             = "COPERNICUS_30",
    spatial_extent = bbox,
    bands          = list("DEM")
  )

  # Reduce temporal dimension first - DEM is static but CDSE may return
  # multiple timestamps. Using max preserves the highest elevation value.
  cube_t <- p$reduce_dimension(
    data      = cube,
    dimension = "t",
    reducer   = function(data, context) { p$max(data = data) }
  )

  # Spatial reducer helper
  sp_reducer <- switch(spatial_reducer,
    mean   = function(data, context) { p$mean(data) },
    median = function(data, context) { p$median(data) },
    min    = function(data, context) { p$min(data) },
    max    = function(data, context) { p$max(data) },
    sum    = function(data, context) { p$sum(data) },
    sd     = function(data, context) { p$sd(data) },
    function(data, context) { p$mean(data) }
  )

  # -- Build a single multi-band cube with all requested terrain parameters ----
  # Merge all parameter cubes into one before aggregate_spatial so only a
  # single compute_result() call is needed regardless of how many parameters
  # are requested. Each band is renamed to its parameter name for clear parsing.
  band_cubes  <- list()
  band_labels <- character(0)

  if (need_elevation) {
    elev_cube <- p$rename_labels(data = cube_t, dimension = "bands",
                                 target = list("elevation"))
    band_cubes[["elevation"]] <- elev_cube
    band_labels <- c(band_labels, "elevation")
  }
  if (need_slope) {
    slope_cube <- p$slope(data = cube_t)
    slope_cube <- p$rename_labels(data = slope_cube, dimension = "bands",
                                  target = list("slope"))
    band_cubes[["slope"]] <- slope_cube
    band_labels <- c(band_labels, "slope")
  }
  if (need_aspect) {
    # p$aspect() returns values in radians (from due North) per openEO spec,
    # so no degree-to-radian conversion is needed for cos/sin.
    # p$cos() and p$sin() operate element-wise on datacubes.
    # Northness/eastness are computed here (before aggregate_spatial) so
    # the spatial reducer sees cos/sin values, not aspect angles:
    # mean(cos(aspect)) != cos(mean(aspect)).
    aspect_cube <- p$aspect(data = cube_t)

    if ("aspect" %in% terrain_params) {
      asp_named <- p$rename_labels(data = aspect_cube, dimension = "bands",
                                   target = list("aspect"))
      band_cubes[["aspect"]] <- asp_named
      band_labels <- c(band_labels, "aspect")
    }

    if ("northness" %in% terrain_params) {
      north_cube <- p$cos(x = aspect_cube)
      north_cube <- p$rename_labels(data = north_cube, dimension = "bands",
                                    target = list("northness"))
      band_cubes[["northness"]] <- north_cube
      band_labels <- c(band_labels, "northness")
    }

    if ("eastness" %in% terrain_params) {
      east_cube <- p$sin(x = aspect_cube)
      east_cube <- p$rename_labels(data = east_cube, dimension = "bands",
                                   target = list("eastness"))
      band_cubes[["eastness"]] <- east_cube
      band_labels <- c(band_labels, "eastness")
    }
  }

  # Merge into one multi-band cube
  combined_cube <- Reduce(
    function(acc, cube) { p$merge_cubes(cube1 = acc, cube2 = cube) },
    band_cubes
  )

  # Single aggregate_spatial + compute_result for all parameters
  notify(paste0("openEO: computing terrain parameters (",
                paste(band_labels, collapse = ", "), ")..."))
  agg         <- p$aggregate_spatial(data = combined_cube,
                                     geometries = geom_json,
                                     reducer = sp_reducer)
  tmp_out     <- tempfile(fileext = ".json")
  result_proc <- p$save_result(agg, format = "JSON")

  tryCatch(
    openeo::compute_result(result_proc, output_file = tmp_out),
    error = function(e) {
      stop("openEO DEM compute_result() failed: ", conditionMessage(e))
    }
  )
  notify("openEO: result received, parsing...")

  raw    <- jsonlite::read_json(tmp_out, simplifyVector = FALSE)
  result <- parse_aggregate_spatial_result(raw, bands = band_labels,
                                           n_features = nrow(y),
                                           time_reducer = "max")
  names(result) <- band_labels

  # -- Assemble output ---------------------------------------------------------
  result_cols <- list()
  if (need_elevation) {
    result_cols[["elevation"]] <- as.numeric(result[["elevation"]])
  }
  if (need_slope) {
    result_cols[["slope"]] <- as.numeric(result[["slope"]])
  }
  if (need_aspect) {
    if ("aspect" %in% terrain_params) {
      result_cols[["aspect"]] <- as.numeric(result[["aspect"]])
    }
    if ("northness" %in% terrain_params) {
      result_cols[["northness"]] <- as.numeric(result[["northness"]])
    }
    if ("eastness" %in% terrain_params) {
      result_cols[["eastness"]] <- as.numeric(result[["eastness"]])
    }
  }

  out <- as.data.frame(result_cols)
  names(out) <- paste0("DEM_", names(out))

  if (!is.null(col_uid) && nchar(col_uid) > 0 && col_uid %in% names(y)) {
    out[[col_uid]] <- as.character(as.data.frame(y)[[col_uid]])
  }

  out
}
