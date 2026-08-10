# =============================================================================
# run_matching.R
#
# Standalone function for running propensity score / covariate matching
# using the MatchIt package.
#
# Usage inside the Shiny app: sourced automatically via global.R.
# Usage outside the app:
#   source("R/run_matching.R")
#   library(MatchIt)
#   library(terra)
#
# Main entry point:
#   run_matching(x, col_treatment, col_covars, ...)
# =============================================================================


# -----------------------------------------------------------------------------
# run_matching()
#
# Constructs the matching formula from the supplied column names, runs
# MatchIt::matchit(), and returns both the matchit object and the matched
# dataset as a SpatVector with an added match-ID attribute.
#
# Arguments:
#   x               SpatVector or data.frame. The input dataset containing
#                   the treatment variable and covariates.
#   col_treatment   Character. Name of the treatment attribute (must be 0/1).
#   col_covars      Character vector. Names of the covariate attributes to
#                   include in the formula.
#   col_uid         Character. Name of a column containing unique feature IDs.
#                   Used to safely re-attach geometry after matching (which
#                   reorders rows). If NULL or not found, a synthetic ID is
#                   created from row names. Strongly recommended when the
#                   input has been subset (e.g. after NA-dropping).
#   ...             Additional arguments passed directly to matchit(), e.g.
#                   method, distance, ratio, replace, caliper, etc.
#                   See ?MatchIt::matchit for the full list.
#
# Returns a named list:
#   $matchit_obj    The matchit object returned by MatchIt::matchit().
#                   Use summary($matchit_obj) to assess balance, or pass
#                   to MatchIt::match.data() / MatchIt::get_matches().
#
#   $matched_data   A SpatVector (or data.frame if x was a data.frame) of
#                   the matched units only, with all original attributes plus:
#                     .weights    Matching weights from matchit.
#                     .subclass   Subclass/pair membership (NA if replaced).
#                     .match_ids  Character. Comma-separated row names of the
#                                 units matched to this unit. For 1:1 matching
#                                 this is a single value; for k:1 or full
#                                 matching it may list several.
#
#   $formula        The formula object passed to matchit(), for reference.
#   $errors         Character vector of non-fatal warnings/messages, or
#                   character(0) if none.
#
# Notes:
#   - MatchIt requires the treatment variable to be numeric 0/1. If the
#     column contains other binary values (e.g. TRUE/FALSE, "treated"/
#     "control") it is coerced with a warning.
#   - Geometry is preserved in $matched_data only when x is a SpatVector.
#     The matched set is the intersection of matched row indices with the
#     original SpatVector rows.
#   - .match_ids is stored as a character column (comma-separated) because
#     SpatVector attributes cannot hold list columns. To recover as a list:
#     strsplit(matched_data$.match_ids, ",")
#
# Examples:
#   v   <- terra::vect("covariates.gpkg")
#   res <- run_matching(v,
#            col_treatment = "treatment",
#            col_covars    = c("ndvi", "elevation", "slope"),
#            method        = "nearest",
#            ratio         = 1,
#            replace       = FALSE)
#   summary(res$matchit_obj)
#   terra::writeVector(res$matched_data, "matched_units.gpkg")
# -----------------------------------------------------------------------------
run_matching <- function(x, col_treatment, col_covars,
                         col_uid     = NULL,
                         treat_value = NULL,
                         ...) {
  
  # -- Validate inputs --------------------------------------------------------
  if (inherits(x, "SpatVector")) {
    is_spat <- TRUE
    df      <- as.data.frame(x)
  } else if (is.data.frame(x)) {
    is_spat <- FALSE
    df      <- x
  } else {
    stop("x must be a SpatVector or data.frame.")
  }
  
  if (!col_treatment %in% names(df)) {
    stop("Treatment column '", col_treatment, "' not found in x.")
  }
  missing_cov <- setdiff(col_covars, names(df))
  if (length(missing_cov) > 0) {
    stop("Covariate column(s) not found in x: ",
         paste(missing_cov, collapse = ", "))
  }
  if (length(col_covars) == 0) {
    stop("At least one covariate must be supplied.")
  }
  
  errors <- character(0)
  
  # -- Coerce treatment to 0/1 using treat_value ----------------------------
  # treat_value: the value in col_treatment that represents treated units (=1).
  # If NULL: use 1 if values are already 0/1, else use the larger of the two
  # unique values (with a warning so the user knows what was assumed).
  treat_raw <- df[[col_treatment]]
  u         <- sort(unique(na.omit(treat_raw)))
  if (length(u) != 2) {
    stop("Treatment variable '", col_treatment,
         "' must have exactly 2 unique non-NA values (found: ",
         length(u), ").")
  }
  # Determine which value maps to 1 (treated)
  if (!is.null(treat_value)) {
    tv <- tryCatch(as.character(treat_value), error = function(e) NULL)
    u_char <- as.character(u)
    if (!tv %in% u_char) {
      stop("treat_value '", treat_value, "' not found in '", col_treatment,
           "'. Available values: ", paste(u, collapse = ", "))
    }
    treat_1_val <- u[u_char == tv]
  } else if (all(u %in% c(0, 1))) {
    treat_1_val <- 1   # standard 0/1 encoding, no message needed
  } else {
    treat_1_val <- u[2]   # default: larger value = treated
    errors <- c(errors, paste0(
      "treat_value not specified; assuming '", treat_1_val,
      "' = treated. Set treat_value explicitly to suppress this warning."))
  }
  treat_0_val <- u[u != treat_1_val]
  if (!all(treat_raw %in% c(treat_0_val, treat_1_val, NA))) {
    stop("Unexpected values in treatment column.")
  }
  df[[col_treatment]] <- as.integer(treat_raw == treat_1_val)
  if (!all(u %in% c(0, 1))) {
    errors <- c(errors, paste0(
      "Treatment coerced to 0/1: '", treat_0_val, "' -> 0, '",
      treat_1_val, "' -> 1."))
  }
  
  # -- Drop rows with missing/non-finite values in covariates or treatment ----
  # matchit() does not handle missingness; we drop incomplete rows and report
  # them as warnings so users can see which units were excluded.
  analysis_cols  <- c(col_treatment, col_covars)
  df_sub         <- df[, analysis_cols, drop = FALSE]
  
  # Identify rows with any NA or non-finite value in the analysis columns
  is_complete <- complete.cases(df_sub)
  has_nonfinite <- apply(df_sub, 1, function(row) {
    nums <- suppressWarnings(as.numeric(row))
    any(is.finite(nums) == FALSE & !is.na(nums))
  })
  keep <- is_complete & !has_nonfinite
  n_dropped <- sum(!keep)
  
  # Per-covariate missingness summary for diagnostics
  miss_by_col <- vapply(analysis_cols, function(col) {
    sum(is.na(df[[col]]) | !is.finite(suppressWarnings(as.numeric(df[[col]]))))
  }, integer(1))
  miss_cols <- names(miss_by_col)[miss_by_col > 0]
  
  dropped_info <- NULL
  if (n_dropped > 0) {
    dropped_idx <- which(!keep)
    # Use UID column for identification if available, else row names
    if (!is.null(col_uid) && col_uid %in% names(df)) {
      dropped_ids <- as.character(df[[col_uid]][dropped_idx])
      id_col_name <- col_uid
    } else {
      dropped_ids <- rownames(df)[dropped_idx]
      id_col_name <- "row_id"
    }
    treat_vals <- df[[col_treatment]][dropped_idx]
    role_vals  <- ifelse(treat_vals == 1, "Treatment", "Control")
    dropped_info <- data.frame(
      ID        = dropped_ids,
      Role      = role_vals,
      reason    = ifelse(!is_complete[dropped_idx], "missing value(s)",
                         "non-finite value(s)"),
      stringsAsFactors = FALSE
    )
    names(dropped_info)[1] <- id_col_name
    errors <- c(errors, paste0(
      n_dropped, " unit(s) dropped before matching due to missing or ",
      "non-finite values in: ", paste(miss_cols, collapse = ", "), "."))
    df <- df[keep, , drop = FALSE]
    if (is_spat) x <- x[keep, ]
  }
  
  # -- Unique ID: create or validate ----------------------------------------
  # A unique ID column is used to safely re-attach geometry to matched_df
  # after matchit() (which reorders/subsets rows). Without a reliable key,
  # row-index matching can silently scramble attributes.
  uid_created <- FALSE
  if (is.null(col_uid) || !col_uid %in% names(df)) {
    # Create a synthetic unique ID from current row names
    col_uid    <- ".uid"
    df$.uid    <- rownames(df)
    uid_created <- TRUE
    if (is_spat) {
      # Add to SpatVector attribute table too
      uid_df         <- as.data.frame(x)
      uid_df$.uid    <- rownames(uid_df)
      terra::values(x) <- uid_df
    }
    if (!uid_created) {
      errors <- c(errors, "No unique ID supplied; auto-generated from row names.")
    }
  } else {
    # Validate: check uniqueness
    uid_vals <- df[[col_uid]]
    if (anyDuplicated(uid_vals) > 0) {
      errors <- c(errors, paste0("Column '", col_uid, "' has duplicate values; ",
                                 "using it as UID may produce incorrect geometry matching."))
    }
  }
  
  # -- Build formula ---------------------------------------------------------
  frm <- as.formula(
    paste0(col_treatment, " ~ ",
           paste(col_covars, collapse = " + "))
  )
  
  # -- Run matchit -----------------------------------------------------------
  m_obj <- tryCatch(
    MatchIt::matchit(formula = frm, data = df, ...),
    error = function(e) {
      stop("matchit() failed: ", conditionMessage(e))
    }
  )
  
  # -- Extract matched data --------------------------------------------------
  # get_matches() returns one row per matched pair per unit (long format).
  # match.data() returns one row per unit with weights and subclass.
  # We use match.data() as the base (preserves original row structure) and
  # derive .match_ids from the match.matrix in the matchit object.
  matched_df <- tryCatch(
    MatchIt::match.data(m_obj, data = df),
    error = function(e) {
      errors <<- c(errors,
                   paste0("match.data() failed: ", conditionMessage(e)))
      NULL
    }
  )
  
  if (is.null(matched_df)) {
    return(list(matchit_obj  = m_obj,
                matched_data = NULL,
                formula      = frm,
                errors       = errors,
                dropped      = dropped_info,
                n_dropped    = n_dropped,
                miss_cols    = miss_cols))
  }
  
  # -- Rename MatchIt-added columns to dot-prefix convention ----------------
  # MatchIt adds: "distance" (propensity score / distance metric),
  #               ".weights" and ".subclass" (already dot-prefixed by MatchIt).
  # We rename "distance" -> ".distance" so users can distinguish app-added
  # columns from their original data attributes.
  if ("distance" %in% names(matched_df)) {
    names(matched_df)[names(matched_df) == "distance"] <- ".distance"
  }
  
  # -- Build .match_ids column using UID values -----------------------------
  # build_match_ids returns matchit internal row names; we translate these
  # to UID values so .match_ids is human-readable and consistent with col_uid.
  match_rn_col <- build_match_ids(m_obj, matched_df)
  
  # Map: matchit row name -> UID value
  # matched_df row names are the matchit row names (keys into match.matrix)
  rn_to_uid <- setNames(
    as.character(matched_df[[col_uid]]),
    rownames(matched_df)
  )
  
  # Translate each comma-separated string of row names to UID values
  match_ids_col <- vapply(match_rn_col, function(rns_str) {
    if (nchar(rns_str) == 0) return("")
    rns  <- strsplit(rns_str, ",")[[1]]
    uids <- rn_to_uid[rns]
    uids <- uids[!is.na(uids)]
    paste(uids, collapse = ",")
  }, character(1))
  
  matched_df$.match_ids <- match_ids_col
  # No .row_id column needed: col_uid is the authoritative key
  
  # -- Sanitise matched_df for terra compatibility ---------------------------
  # terra::values<- requires a plain data frame with only atomic columns.
  # match.data() adds .subclass (factor) and .weights (numeric); factors
  # must be converted to character, and any list columns dropped.
  sanitise_for_terra <- function(df_in) {
    for (col in names(df_in)) {
      x_col <- df_in[[col]]
      if (is.factor(x_col)) {
        df_in[[col]] <- as.character(x_col)
      } else if (is.list(x_col)) {
        # Convert list columns to comma-separated character
        df_in[[col]] <- vapply(x_col, function(v) {
          paste(as.character(v), collapse = ",")
        }, character(1))
      } else if (!is.atomic(x_col)) {
        df_in[[col]] <- as.character(x_col)
      }
    }
    df_in
  }
  
  matched_df_clean <- sanitise_for_terra(matched_df)
  
  # -- Attach geometry using UID as the join key ----------------------------
  # This is the safe approach: we never rely on row index ordering.
  # matched_df_clean rows are joined to x geometries via col_uid.
  if (is_spat) {
    # Build a geometry-only SpatVector with the UID column
    geom_df      <- as.data.frame(x)             # attributes of NA-dropped x
    geom_df$.wkt <- terra::geom(x, wkt = TRUE)   # one WKT string per feature
    
    # Join matched attributes onto geometry via UID
    merged <- merge(geom_df[, c(col_uid, ".wkt")],
                    matched_df_clean,
                    by      = col_uid,
                    all.x   = FALSE,
                    all.y   = FALSE,
                    sort    = FALSE)
    
    if (nrow(merged) == 0) {
      stop("UID-based geometry merge produced 0 rows. ",
           "Check that '", col_uid, "' is present in both datasets.")
    }
    
    wkt_col <- merged$.wkt
    merged$.wkt <- NULL
    rownames(merged) <- NULL
    
    out <- terra::vect(cbind(merged, geometry = wkt_col),
                       geom = "geometry",
                       crs  = terra::crs(x))
  } else {
    out <- matched_df_clean
  }
  
  list(
    matchit_obj  = m_obj,
    matched_data = out,
    formula      = frm,
    errors       = errors,
    dropped      = dropped_info,   # data.frame of dropped units, or NULL
    n_dropped    = n_dropped,
    miss_cols    = miss_cols       # covariate names with missingness
  )
}


# -----------------------------------------------------------------------------
# build_match_ids()
#
# Internal helper. Derives a character vector of comma-separated matchit
# internal row names from the match.matrix or subclass components.
# The caller translates these row names to UID values.
#
# For treated units: IDs are the matched control row names.
# For control units: IDs are the treated row names they were matched to.
# Unmatched units: empty string "".
# -----------------------------------------------------------------------------
build_match_ids <- function(m_obj, matched_df) {
  
  mm <- m_obj$match.matrix  # NULL for some methods (full, cem, exact, subclass)
  
  n      <- nrow(matched_df)
  result <- rep("", n)
  rn     <- rownames(matched_df)
  
  if (!is.null(mm)) {
    # mm rows = treated unit names; values = matched control names (or NA)
    treated_names <- rownames(mm)
    
    # For each row in matched_df, look up its match partners
    for (k in seq_len(n)) {
      unit <- rn[k]
      if (unit %in% treated_names) {
        # This is a treated unit: partners are the controls in mm[unit, ]
        partners <- mm[unit, ]
        partners <- partners[!is.na(partners)]
      } else {
        # This is a control unit: find which treated rows matched to it
        partners <- treated_names[apply(mm, 1, function(row) unit %in% row)]
      }
      result[k] <- paste(partners, collapse = ",")
    }
    
  } else if (!is.null(m_obj$subclass)) {
    # For subclassification / full / cem: group by subclass
    sub <- m_obj$subclass          # named vector, names = all unit row names
    treat_vec <- m_obj$treat       # 0/1, same names
    
    for (k in seq_len(n)) {
      unit <- rn[k]
      if (!unit %in% names(sub)) next
      sc <- sub[unit]
      if (is.na(sc)) next
      # Partners = units in same subclass with opposite treatment status
      same_sc    <- names(sub)[!is.na(sub) & sub == sc]
      opp_treat  <- if (treat_vec[unit] == 1) 0 else 1
      partners   <- same_sc[treat_vec[same_sc] == opp_treat]
      result[k]  <- paste(partners, collapse = ",")
    }
  }
  
  result
}