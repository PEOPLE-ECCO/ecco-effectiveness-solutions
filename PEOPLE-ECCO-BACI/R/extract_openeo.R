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
                           backend_url     = "https://openeo.dataspace.copernicus.eu") {
  
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
  if (!terra::is.lonlat(y)) {
    y_wgs <- terra::project(y, "EPSG:4326")
  } else {
    y_wgs <- y
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
  # Strip all attributes before export to minimise GeoJSON payload size.
  # Only geometry (and a minimal feature ID) is needed by aggregate_spatial.
  # A temporary sequential UID is kept so result rows can be aligned back
  # to the original features after extraction.
  y_geom          <- y_wgs
  y_geom$.tmp_uid <- as.character(seq_len(nrow(y_geom)))
  # Keep only the temp UID — drops all other attributes
  y_geom          <- y_geom[, ".tmp_uid"]
  
  tmp_geojson <- tempfile(fileext = ".geojson")
  terra::writeVector(y_geom, tmp_geojson, filetype = "GeoJSON", overwrite = TRUE)
  geom_json <- jsonlite::read_json(tmp_geojson)
  
  if (!identical(geom_json$type, "FeatureCollection")) {
    stop("GeoJSON export did not produce a FeatureCollection. ",
         "Got type: '", geom_json$type, "'")
  }
  
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
  
  # CDSE supports JSON output for aggregate_spatial results
  result_process <- p$save_result(aggregated, format = "JSON")
  
  # -- Execute synchronously -------------------------------------------------
  tmp_result <- tempfile(fileext = ".json")
  message("Submitting openEO process graph to ", backend_url, "...")
  tryCatch(
    openeo::compute_result(result_process, output_file = tmp_result),
    error = function(e) {
      # Try to give a cleaner error message
      msg <- conditionMessage(e)
      stop("openEO compute_result() failed: ", msg)
    }
  )
  message("openEO result received.")
  
  # -- Parse result ----------------------------------------------------------
  raw <- jsonlite::read_json(tmp_result, simplifyVector = TRUE)
  
  # aggregate_spatial returns a list: one entry per feature, each a list of
  # band values. Structure depends on backend; handle both flat and nested.
  result_df <- parse_aggregate_spatial_result(raw, bands, nrow(y))
  
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
# parse_aggregate_spatial_result()
#
# Internal helper: parses the JSON output of aggregate_spatial into a
# data.frame with one row per feature and one column per band.
# Handles the nested list structure returned by CDSE.
# -----------------------------------------------------------------------------
parse_aggregate_spatial_result <- function(raw, bands, n_features) {
  
  # CDSE returns: list of per-feature lists, each containing band values.
  # Two common structures:
  # A) [[feature]][[band_index]] = value  (list of lists)
  # B) data.frame with columns per band   (if simplifyVector worked)
  
  if (is.data.frame(raw)) {
    return(raw)
  }
  
  if (is.list(raw)) {
    # Try to coerce: each element should be a numeric vector (one per band)
    rows <- lapply(raw, function(feat) {
      vals <- unlist(feat, use.names = FALSE)
      if (length(vals) == 0) vals <- rep(NA_real_, max(length(bands), 1))
      as.data.frame(t(vals))
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }
  
  # Fallback: return NAs
  warning("Could not parse openEO aggregate_spatial result; returning NAs.")
  as.data.frame(matrix(NA_real_, nrow = n_features,
                       ncol = max(length(bands), 1)))
}