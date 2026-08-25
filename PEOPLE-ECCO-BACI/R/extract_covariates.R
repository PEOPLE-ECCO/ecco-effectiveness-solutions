# =============================================================================
# extract_covariates.R
#
# Standalone functions for extracting covariate values from spatial sources
# and merging them with an input SpatVector.
#
# Usage inside the Shiny app: sourced automatically via global.R.
# Usage outside the app:
#   source("R/extract_covariates.R")
#   library(terra)
#
# Main entry point:
#   run_extraction(vect, tab2_attrs, tab2_ci, tab2_uid,
#                  matchlyr_list, input_vals, progress)
# =============================================================================


# -----------------------------------------------------------------------------
# make_colname()
#
# Sanitises a string for use as a column name: replaces any run of
# non-alphanumeric characters with a single underscore, and strips
# leading/trailing underscores.
# -----------------------------------------------------------------------------
make_colname <- function(x) {
  x <- gsub("[^A-Za-z0-9]", "_", x)
  x <- gsub("_+", "_", x)
  x <- sub("^_|_$", "", x)
  x
}


# -----------------------------------------------------------------------------
# make_fraction_fun()
#
# Parses a user-supplied condition string (e.g. "x > 30" or "x %in% 22:29")
# into a function suitable for terra::extract(fun = ...).
#
# The resulting function computes the fraction of (non-NA) extracted pixel
# values satisfying the condition:
#
#   na.rm = TRUE  (default): denominator = number of non-NA pixels
#   na.rm = FALSE:           denominator = total number of pixels
#
# This is the ONLY case where a wrapper function is necessary - terra has no
# built-in "fraction" method, so we must construct the function ourselves.
# -----------------------------------------------------------------------------
make_fraction_fun <- function(condition_str, na.rm = TRUE) {
  condition_str <- trimws(condition_str)
  if (nchar(condition_str) == 0) {
    stop("Fraction condition is empty. Please enter a condition such as 'x > 30'.")
  }

  # Validate by attempting a dry-run parse
  test <- tryCatch(
    eval(parse(text = paste0("function(x) x ", condition_str))),
    error = function(e) stop("Cannot parse condition '", condition_str,
                             "': ", conditionMessage(e))
  )

  if (na.rm) {
    fun_text <- paste0(
      "function(x) {",
      "  x <- x[!is.na(x)];",
      "  if (length(x) == 0) return(NA_real_);",
      "  sum(x ", condition_str, ") / length(x)",
      "}"
    )
  } else {
    fun_text <- paste0(
      "function(x) {",
      "  if (length(x) == 0) return(NA_real_);",
      "  sum(x ", condition_str, ", na.rm = TRUE) / length(x)",
      "}"
    )
  }

  eval(parse(text = fun_text))
}


# -----------------------------------------------------------------------------
# extract_local_raster()
#
# Extracts values from a raster for all features in a SpatVector.
# Signature mirrors terra::extract(x, y, fun, method, ...) as closely as
# possible.
#
# Arguments:
#   x              SpatRaster  or character path / URL to a GeoTIFF.
#                  If a path is supplied the raster is opened here.
#   y              SpatVector  input features.
#   fun            The function to summarise extracted pixel values per feature.
#                  Accepts:
#                    - NULL (default): return all pixel values (terra default)
#                    - A named string: "mean", "median", "sd", "min", "max",
#                      "sum" - passed directly to terra::extract as the
#                      corresponding base-R function, so na.rm is forwarded
#                      via ... as terra::extract(x, y, fun = mean, na.rm = TRUE)
#                    - "fraction": uses make_fraction_fun(); requires
#                      fun_fraction (condition string) and optionally na.rm
#                    - "custom": evaluates fun_custom as an R function string
#                    - An actual R function: passed straight through to terra
#   method         "simple" (default) or "bilinear". Used for point extraction
#                  and for polygon/line extraction when fun is "simple" or
#                  "bilinear" (centroid-based extraction).
#   sel_lyr        Integer or character vector of layer indices to subset.
#                  NULL = all layers.
#   fun_fraction   Character string condition for fun = "fraction",
#                  e.g. "x > 30" or "x %in% 22:29".
#   fun_custom     Character string defining an R function for fun = "custom",
#                  e.g. "function(x) quantile(x, 0.9, na.rm = TRUE)".
#   layer_prefix   Character prefix for output column names.
#                  Defaults to the filename without extension.
#   ...            Further arguments forwarded to terra::extract(), e.g.
#                  na.rm = TRUE, weights = TRUE.
#
# Returns:
#   A data.frame with nrow(y) rows and one column per selected layer.
#   Column names follow the pattern <layer_prefix>_<layer_name>.
# -----------------------------------------------------------------------------
extract_local_raster <- function(x,
                                 y,
                                 fun          = NULL,
                                 method       = "simple",
                                 sel_lyr      = NULL,
                                 fun_fraction = NULL,
                                 fun_custom   = NULL,
                                 layer_prefix = NULL,
                                 use_chunks   = FALSE,
                                 chunk_size   = 500L,
                                 ...) {

  # -- Open raster if a path was supplied -------------------------------------
  if (is.character(x)) {
    path <- x
    x    <- terra::rast(x)
  } else {
    path <- NULL
  }

  # -- Subset layers ----------------------------------------------------------
  # sel_lyr can be integer indices or layer name strings; handle both.
  if (!is.null(sel_lyr) && length(sel_lyr) > 0) {
    idx <- suppressWarnings(as.integer(sel_lyr))
    if (any(is.na(idx))) {
      # Names provided: match to actual layer names
      idx <- match(sel_lyr, names(x))
      idx <- idx[!is.na(idx)]
    }
    if (length(idx) > 0) {
      x <- terra::subset(x, idx)
    }
  }

  # -- Column name prefix -----------------------------------------------------
  if (is.null(layer_prefix) || nchar(layer_prefix) == 0) {
    layer_prefix <- if (!is.null(path)) {
      tools::file_path_sans_ext(basename(path))
    } else {
      "raster"
    }
  }
  layer_prefix <- make_colname(layer_prefix)

  # -- Resolve geometry -------------------------------------------------------
  geom_type <- unique(terra::geomtype(y))
  is_point  <- length(geom_type) == 1 && geom_type %in% c("points", "point")

  # "simple"/"bilinear" as fun means centroid extraction for polygons/lines
  use_centroid <- !is_point &&
                  is.character(fun) &&
                  fun %in% c("simple", "bilinear")

  if (use_centroid) {
    y      <- terra::centroids(y)
    method <- fun          # use as the terra method argument
    fun    <- NULL         # no summary function needed
  }

  # -- Resolve fun argument ---------------------------------------------------
  # Only the two special cases (fraction, custom) need explicit handling.
  # For everything else, match.fun() looks up the function by name in the
  # base/terra namespace, so any R function - mean, median, sd, var, modal,
  # quantile, etc. - is accepted without listing them here.
  terra_fun <- if (is.null(fun) || identical(fun, "")) {
    NULL

  } else if (is.function(fun)) {
    fun   # already an R function, pass straight through

  } else if (is.character(fun)) {
    if (fun == "fraction") {
      if (is.null(fun_fraction) || nchar(trimws(fun_fraction)) == 0) {
        stop("fun = 'fraction' requires a non-empty fun_fraction condition string.")
      }
      dots      <- list(...)
      na_rm_val <- if ("na.rm" %in% names(dots)) dots[["na.rm"]] else TRUE
      make_fraction_fun(fun_fraction, na.rm = na_rm_val)

    } else if (fun == "custom") {
      if (is.null(fun_custom) || nchar(trimws(fun_custom)) == 0) {
        stop("fun = 'custom' requires a non-empty fun_custom function string.")
      }
      tryCatch(
        eval(parse(text = fun_custom)),
        error = function(e) stop("Cannot parse custom function: ", conditionMessage(e))
      )

    } else {
      # For any other string, look up the function by name.
      # This covers mean, median, sd, min, max, sum, var, modal, etc.
      # An informative error is raised if the name does not resolve.
      tryCatch(
        match.fun(fun),
        error = function(e) stop(
          "fun = '", fun, "' could not be resolved to an R function. ",
          "Check the spelling or pass the function directly, e.g. fun = mean."
        )
      )
    }

  } else {
    stop("fun must be NULL, a character string, or an R function.")
  }

  # -- Align CRS: reproject y to match x if needed ---------------------------
  # terra::extract() requires both objects to share the same CRS.
  # We reproject y (the vector) rather than x (the raster) to avoid
  # resampling the raster pixel values.
  if (!terra::same.crs(x, y)) {
    message("CRS mismatch: reprojecting input vector to match raster CRS.")
    y <- terra::project(y, terra::crs(x))
  }

  # -- Extract ----------------------------------------------------------------
  n          <- nrow(y)
  chunk_size <- if (is.null(chunk_size) || is.na(chunk_size)) { 500L } else { as.integer(chunk_size) }

  if (use_chunks && n > chunk_size) {
    chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
    result <- do.call(rbind, lapply(chunks, function(idx) {
      terra::extract(x, y[idx, ], fun = terra_fun, method = method,
                     ID = FALSE, ...)
    }))
  } else {
    result <- terra::extract(x, y, fun = terra_fun, method = method,
                             ID = FALSE, ...)
  }

  # -- Rename columns ---------------------------------------------------------
  safe_names <- paste0(layer_prefix, "_", vapply(names(x), make_colname, character(1)))
  names(result) <- safe_names

  result
}


# -----------------------------------------------------------------------------
# extract_local_vector()
#
# Extracts covariate values from a covariate SpatVector for all features in
# the input SpatVector y.
#
# Arguments:
#   x            SpatVector or character path to covariate vector file.
#   y            SpatVector input features.
#   fun          Character: "mean","median","sd","min","max","sum",
#                "count","minDistance".
#   vec_attrs    Character vector of attribute names to summarise.
#                NULL = all numeric attributes.
#   layer_prefix Character prefix for output column names.
#   ...          Further arguments forwarded where applicable (e.g. na.rm).
#
# Returns a data.frame with nrow(y) rows.
# -----------------------------------------------------------------------------
extract_local_vector <- function(x,
                                 y,
                                 fun          = "mean",
                                 vec_attrs    = NULL,
                                 layer_prefix = NULL,
                                 ...) {

  if (is.character(x)) {
    path <- x
    x    <- terra::vect(x)
  } else {
    path <- NULL
  }

  if (is.null(layer_prefix) || nchar(layer_prefix) == 0) {
    layer_prefix <- if (!is.null(path)) {
      tools::file_path_sans_ext(basename(path))
    } else {
      "vector"
    }
  }
  layer_prefix <- make_colname(layer_prefix)

  dots  <- list(...)
  na_rm <- if ("na.rm" %in% names(dots)) dots[["na.rm"]] else TRUE

  # -- Align CRS: reproject y to match x if needed ---------------------------
  if (!terra::same.crs(x, y)) {
    message("CRS mismatch: reprojecting input vector to match covariate vector CRS.")
    y <- terra::project(y, terra::crs(x))
  }

  if (fun == "count") {
    rel    <- terra::relate(y, x, relation = "intersects")
    counts <- rowSums(rel)
    out    <- data.frame(count = counts)
    names(out) <- paste0(layer_prefix, "_count")
    return(out)
  }

  if (fun == "minDistance") {
    # Use sf + s2/GEOS with spatial indexing instead of terra::distance().
    # terra computes a full n*m pairwise matrix without indexing - prohibitively
    # slow for large datasets. sf::st_nearest_feature uses an R-tree index
    # (O(n log m)) then st_distance only computes the n needed distances.
    #
    # CRS handling:
    #   lon/lat  -> sf uses the s2 spherical engine automatically (distances in m)
    #   projected -> sf uses GEOS planar engine (distances in CRS units, typically m)
    # Both are consistent; no manual unit conversion needed.

    # Convert to sf, ensuring both share the same CRS
    y_sf <- sf::st_as_sf(y)
    x_sf <- sf::st_as_sf(x)

    if (!sf::st_crs(y_sf) == sf::st_crs(x_sf)) {
      x_sf <- sf::st_transform(x_sf, sf::st_crs(y_sf))
    }

    # Find nearest feature in x for each feature in y (spatial index)
    nearest_idx <- sf::st_nearest_feature(y_sf, x_sf)

    # Compute only the n required distances (by_element avoids n*m matrix)
    min_d <- sf::st_distance(y_sf, x_sf[nearest_idx, ], by_element = TRUE)

    # st_distance returns a units object; strip to plain numeric (metres)
    min_d_num <- as.numeric(min_d)

    out        <- data.frame(minDistance = min_d_num)
    names(out) <- paste0(layer_prefix, "_minDistance")
    return(out)
  }

  # -- Attribute-based summary ------------------------------------------------
  # Geometry-aware: behaviour depends on the combination of y (units) and x
  # (covariate) geometry types.
  #
  # Points-in-polygons (y=polygons, x=points):
  #   terra::extract() aggregates point attributes falling within each polygon.
  #   This is efficient and correct.
  #
  # Lines or polygons summarised by polygon (y=polygons, x=lines/polygons):
  #   terra::extract(SpatVector, SpatVector) is not well-defined for these
  #   combinations. Instead: sf::st_intersection() to find overlapping pairs,
  #   then summarise attribute values per unit of analysis.
  #
  # Points summarised by point (y=points, x=points):
  #   Nearest-neighbour join via sf::st_nearest_feature().

  all_attrs <- names(x)
  if (!is.null(vec_attrs) && length(vec_attrs) > 0) {
    use_attrs <- intersect(vec_attrs, all_attrs)
  } else {
    df_cov    <- as.data.frame(x)
    use_attrs <- all_attrs[vapply(df_cov, is.numeric, logical(1))]
  }
  if (length(use_attrs) == 0) {
    stop("No numeric attributes available in covariate vector for fun = '",
         fun, "'.")
  }

  agg_fun <- switch(fun,
    mean   = function(v) mean(v,   na.rm = na_rm),
    median = function(v) median(v, na.rm = na_rm),
    sd     = function(v) sd(v,     na.rm = na_rm),
    min    = function(v) min(v,    na.rm = na_rm),
    max    = function(v) max(v,    na.rm = na_rm),
    sum    = function(v) sum(v,    na.rm = na_rm),
    stop("Unknown fun = '", fun, "'.")
  )

  y_geom_type <- unique(as.character(terra::geomtype(y)))
  x_geom_type <- unique(as.character(terra::geomtype(x)))
  y_is_poly   <- any(y_geom_type %in% c("polygons"))
  x_is_point  <- any(x_geom_type %in% c("points"))

  if (y_is_poly && x_is_point) {
    # -- Points in polygons: terra::extract is efficient and correct -----------
    x_sub  <- x[, use_attrs]
    result <- terra::extract(x_sub, y,
                             fun   = function(v) agg_fun(v),
                             ID    = FALSE,
                             na.rm = na_rm)
  } else {
    # -- Lines/polygons in polygons, or point-to-point: use sf intersection ---
    # Convert to sf in a common CRS
    y_sf <- sf::st_as_sf(y)
    x_sf <- sf::st_as_sf(x)[, use_attrs]
    if (!sf::st_crs(y_sf) == sf::st_crs(x_sf)) {
      x_sf <- sf::st_transform(x_sf, sf::st_crs(y_sf))
    }

    if (y_is_poly) {
      # Intersection: assigns each x-feature to the y-polygon it overlaps
      suppressWarnings(
        inter <- sf::st_intersection(
          x_sf,
          y_sf[, c(intersect(c(".uid", col_uid), names(y_sf))[1])]
        )
      )
      # .uid or first column is the polygon ID
      grp_col <- names(inter)[ncol(inter)]  # last col added by intersection
      inter_df <- sf::st_drop_geometry(inter)
      # Aggregate per polygon
      result <- do.call(rbind, lapply(seq_len(nrow(y)), function(i) {
        uid_val <- as.character(sf::st_drop_geometry(y_sf)[i, 1, drop = TRUE])
        rows    <- inter_df[as.character(inter_df[[grp_col]]) == uid_val, ,
                            drop = FALSE]
        as.data.frame(lapply(use_attrs, function(a) {
          vals <- suppressWarnings(as.numeric(rows[[a]]))
          if (length(vals) == 0) { NA_real_ } else { agg_fun(vals) }
        }))
      }))
      names(result) <- use_attrs
    } else {
      # Point-to-point or point-to-line: nearest-neighbour join
      nn_idx  <- sf::st_nearest_feature(y_sf, x_sf)
      nn_df   <- sf::st_drop_geometry(x_sf[nn_idx, , drop = FALSE])
      result  <- as.data.frame(lapply(use_attrs, function(a) {
        as.numeric(nn_df[[a]])
      }))
      names(result) <- use_attrs
    }
  }

  rownames(result) <- NULL
  safe_names <- paste0(layer_prefix, "_", fun, "_",
                       vapply(use_attrs, make_colname, character(1)))
  names(result) <- safe_names
  result
}


# -----------------------------------------------------------------------------
# run_extraction()
#
# Orchestrates the full covariate extraction pipeline. Combines a set of
# pre-selected attributes from the input vector with values extracted from
# one or more external spatial sources, and returns a SpatVector.
#
# This function has no dependency on Shiny and can be used in any R script.
#
# Arguments:
#   y              SpatVector. The input feature set. Geometry is preserved
#                  in the output.
#
#   base_attrs     Character vector of attribute names already present in y
#                  to carry through to the output (e.g. treatment column,
#                  unique ID, matching covariates from the same file).
#                  NULL or empty = no attributes from y are kept.
#
#   sources        A list of source specifications. Each element is a named
#                  list describing one external covariate layer. The fields
#                  recognised per source type are:
#
#                  Raster source (type = "raster"):
#                    $type          "raster"
#                    $path          Character. File path or URL to GeoTIFF.
#                                   For URLs, prefix /vsicurl/ yourself, or
#                                   set $url = TRUE to have it added.
#                    $url           Logical. If TRUE, /vsicurl/ is prepended
#                                   to $path. Default FALSE.
#                    $sel_lyr       Integer/character vector of layer indices.
#                                   NULL = all layers.
#                    $fun           Character or R function. Extraction
#                                   function passed to extract_local_raster().
#                                   See that function for accepted values.
#                    $method        "simple" (default) or "bilinear".
#                    $fun_fraction  Condition string for fun = "fraction".
#                    $fun_custom    Function string for fun = "custom".
#                    $na.rm         Logical. Default TRUE.
#                    $layer_prefix  Column name prefix. Default: filename.
#
#                  Vector source (type = "vector"):
#                    $type          "vector"
#                    $path          Character. File path to vector file.
#                    $fun           Character summary function name.
#                                   See extract_local_vector().
#                    $attrs         Character vector of attributes to
#                                   summarise. NULL = all numeric.
#                    $na.rm         Logical. Default TRUE.
#                    $layer_prefix  Column name prefix. Default: filename.
#
#   progress_fun   Optional function(message, fraction) called at each step
#                  to report progress. fraction is in [0, 1]. Supply any
#                  callback you like - a Shiny progress object, a
#                  message() call, a progress bar package, etc.
#                  NULL (default) = silent.
#
# Returns a named list:
#   $result  SpatVector with the geometry of y and all extracted attributes.
#   $errors  Character vector of non-fatal per-source error messages.
#            Sources that fail are skipped; processing continues.
#
# Examples:
#   # Minimal: carry treatment column, add one raster mean
#   sources <- list(
#     list(type = "raster", path = "forest_cover.tif",
#          fun = "mean", na.rm = TRUE)
#   )
#   out <- run_extraction(y = my_vect, base_attrs = "treatment",
#                         sources = sources)
#   terra::writeVector(out$result, "covariates.gpkg")
#
#   # With a progress callback using message()
#   out <- run_extraction(y, base_attrs, sources,
#                         progress_fun = function(msg, frac)
#                           message(sprintf("[%.0f%%] %s", frac * 100, msg)))
# -----------------------------------------------------------------------------
run_extraction <- function(y,
                           base_attrs   = NULL,
                           sources      = list(),
                           col_uid      = NULL,
                           progress_fun = NULL) {

  n_steps <- 1 + length(sources)

  report <- function(msg, step) {
    if (!is.null(progress_fun)) progress_fun(msg, step / n_steps)
  }

  # -- Step 1: subset base attributes from y ----------------------------------
  report("Collecting base attributes...", 0)

  keep_cols <- unique(base_attrs)
  keep_cols <- keep_cols[!vapply(keep_cols, is.null, logical(1)) &
                          nchar(keep_cols) > 0]
  keep_cols <- intersect(keep_cols, names(y))

  base_df <- if (length(keep_cols) > 0) {
    as.data.frame(y)[, keep_cols, drop = FALSE]
  } else {
    data.frame(row.names = seq_len(nrow(y)))
  }

  all_dfs <- list(base_df)
  errors  <- character(0)

  # -- Step 2+: iterate over external sources ---------------------------------
  for (i in seq_along(sources)) {
    src <- sources[[i]]

    src_label <- if (!is.null(src$layer_prefix) && nchar(src$layer_prefix) > 0) {
      src$layer_prefix
    } else if (!is.null(src$path)) {
      tools::file_path_sans_ext(basename(src$path))
    } else {
      paste0("source_", i)
    }

    report(paste0("Extracting: ", src_label, "..."), i)

    result_df <- tryCatch({

      if (identical(src$type, "raster")) {
        rast_path <- if (isTRUE(src$url)) {
          paste0("/vsicurl/", src$path)
        } else {
          src$path
        }
        na_rm <- if (!is.null(src$na.rm)) src$na.rm else TRUE

        extract_local_raster(
          x            = rast_path,
          y            = y,
          fun          = src$fun,
          method       = if (!is.null(src$method)) src$method else "simple",
          sel_lyr      = src$sel_lyr,
          fun_fraction = src$fun_fraction,
          fun_custom   = src$fun_custom,
          layer_prefix = src$layer_prefix,
          use_chunks   = isTRUE(src$use_chunks),
          chunk_size   = src$chunk_size,
          na.rm        = na_rm
        )

      } else if (identical(src$type, "vector_merge")) {
        # Merge: join attributes from covariate vector onto y by geometry or ID.
        x_cov    <- terra::vect(src$path)
        attrs    <- src$merge_attrs
        merge_by <- if (!is.null(src$merge_by)) { src$merge_by } else { "geometry" }
        prefix   <- make_colname(src$layer_prefix)
        if (is.null(attrs) || length(attrs) == 0) { attrs <- names(x_cov) }
        keep_cols <- intersect(attrs, names(x_cov))
        if (length(keep_cols) == 0) {
          stop("No matching attributes found for merge.")
        }

        if (merge_by == "id") {
          id_field <- src$merge_id_field
          if (is.null(id_field) || !id_field %in% names(x_cov)) {
            stop("Merge by ID: field '", id_field,
                 "' not found in covariate vector.")
          }
          if (is.null(col_uid) || !col_uid %in% names(y)) {
            stop("Merge by ID: no UID column '", col_uid, "' in units vector.")
          }
          y_ids  <- as.character(as.data.frame(y)[[col_uid]])
          cov_df <- as.data.frame(x_cov)
          cov_df[[id_field]] <- as.character(cov_df[[id_field]])
          matched <- cov_df[match(y_ids, cov_df[[id_field]]),
                            keep_cols, drop = FALSE]
          rownames(matched) <- NULL
          names(matched) <- paste0(prefix, "_",
                                   vapply(keep_cols, make_colname, character(1)))
          matched

        } else {
          # Geometry join: nearest-feature (centroid-to-centroid via sf index)
          if (!terra::same.crs(x_cov, y)) {
            x_cov <- terra::project(x_cov, terra::crs(y))
          }
          y_sf   <- sf::st_as_sf(y)
          x_sf   <- sf::st_as_sf(x_cov)[, keep_cols, drop = FALSE]
          nn_idx <- sf::st_nearest_feature(y_sf, x_sf)
          result <- sf::st_drop_geometry(x_sf[nn_idx, , drop = FALSE])
          rownames(result) <- NULL
          names(result) <- paste0(prefix, "_",
                                  vapply(keep_cols, make_colname, character(1)))
          result
        }

      } else if (identical(src$type, "vector")) {
        na_rm <- if (!is.null(src$na.rm)) src$na.rm else TRUE

        extract_local_vector(
          x            = src$path,
          y            = y,
          fun          = if (!is.null(src$fun)) src$fun else "mean",
          vec_attrs    = src$attrs,
          layer_prefix = src$layer_prefix,
          na.rm        = na_rm
        )

      } else if (identical(src$type, "openeo")) {
        con <- tryCatch(
          openeo_connect(
            if (!is.null(src$backend_url)) { src$backend_url } else {
              "https://openeo.dataspace.copernicus.eu"
            }
          ),
          error = function(e) {
            errors <<- c(errors, paste0("[", src_label, "] Auth: ",
                                        conditionMessage(e)))
            NULL
          }
        )
        if (is.null(con)) {
          NULL
        } else {
          tryCatch(
            extract_openeo(
              y               = y,
              collection      = src$collection,
              bands           = src$bands,
              spatial_reducer = if (!is.null(src$reducer)) { src$reducer } else { "mean" },
              col_uid         = col_uid,
              layer_prefix    = src$layer_prefix,
              con             = con,
              progress_fun    = progress_fun
            ),
            error = function(e) {
              errors <<- c(errors, paste0("[", src_label, "] ",
                                          conditionMessage(e)))
              NULL
            }
          )
        }
      } else {
        warning("Source ", i, " has unknown type '", src$type, "'. Skipping.")
        NULL
      }

    }, error = function(e) {
      errors <<- c(errors, paste0("[", src_label, "] ", conditionMessage(e)))
      NULL
    })

    if (!is.null(result_df)) {
      all_dfs <- c(all_dfs, list(result_df))
    }
  }

  # -- Combine and attach geometry --------------------------------------------
  combined_df <- do.call(cbind, all_dfs)

  # terra::setValues() is for SpatRaster. For SpatVector, assign the
  # data frame via values<-(), which replaces all attributes while
  # keeping the geometry intact.
  out_vect          <- y
  terra::values(out_vect) <- combined_df

  list(result = out_vect, errors = errors)
}
