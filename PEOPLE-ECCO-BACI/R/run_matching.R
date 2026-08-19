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
  
  # -- Step 1: Validate and assign unique ID ---------------------------------
  # Done first so the UID is available for dropped-unit reporting.
  # If col_uid is provided, it must exist and have unique values (fatal error).
  # If not provided, create .uid as sequential character IDs.
  if (!is.null(col_uid) && nchar(col_uid) > 0) {
    if (!col_uid %in% names(df)) {
      stop("UID column '", col_uid, "' not found in x.")
    }
    uid_vals <- df[[col_uid]]
    if (anyDuplicated(uid_vals) > 0) {
      stop("UID column '", col_uid, "' has duplicate values. ",
           "A unique identifier is required for safe geometry merging.")
    }
  } else {
    col_uid    <- ".uid"
    df$.uid    <- as.character(seq_len(nrow(df)))
    if (is_spat) {
      # Add .uid to the SpatVector attribute table as well
      x[[".uid"]] <- as.character(seq_len(nrow(x)))
    }
    errors <- c(errors, "No unique ID supplied; auto-generated sequentially.")
  }
  
  # -- Step 2: Coerce treatment to 0/1 using treat_value --------------------
  # treat_value: the value in col_treatment representing treated units (= 1).
  # One value = treated; all other values = control (supports multi-level
  # control groups, e.g. several province names as control).
  # If NULL and values are not already 0/1, the maximum value is assumed treated.
  treat_raw <- df[[col_treatment]]
  u         <- sort(unique(na.omit(treat_raw)))
  if (length(u) < 2) {
    stop("Treatment variable '", col_treatment,
         "' must have at least 2 unique non-NA values (found: ",
         length(u), ").")
  }
  if (!is.null(treat_value)) {
    tv     <- as.character(treat_value)
    u_char <- as.character(u)
    if (!tv %in% u_char) {
      stop("treat_value '", treat_value, "' not found in '", col_treatment,
           "'. Available values: ", paste(u, collapse = ", "))
    }
    treat_1_val <- u[u_char == tv]
  } else if (all(u %in% c(0, 1))) {
    treat_1_val <- 1
  } else {
    treat_1_val <- max(u)
    errors <- c(errors, paste0(
      "treat_value not specified; assuming '", treat_1_val,
      "' = treated. Set treat_value explicitly to suppress this warning."))
  }
  # Internal 0/1 column for matchit; original values preserved in output
  df[[".treat_01"]] <- as.integer(as.character(treat_raw) ==
                                    as.character(treat_1_val))
  if (!(length(u) == 2 && all(u %in% c(0, 1)))) {
    errors <- c(errors, paste0(
      "Treatment encoded internally: '", treat_1_val,
      "' = treated (1); all other values = control (0). ",
      "Original values are preserved in the output."))
  }
  
  # -- Step 3: Drop rows with missing/non-finite values ----------------------
  analysis_cols <- c(col_treatment, col_covars)
  df_sub        <- df[, analysis_cols, drop = FALSE]
  
  is_complete   <- complete.cases(df_sub)
  has_nonfinite <- apply(df_sub, 1, function(row) {
    nums <- suppressWarnings(as.numeric(row))
    any(!is.finite(nums) & !is.na(nums))
  })
  keep      <- is_complete & !has_nonfinite
  n_dropped <- sum(!keep)
  
  miss_by_col <- vapply(analysis_cols, function(col) {
    sum(is.na(df[[col]]) |
          !is.finite(suppressWarnings(as.numeric(df[[col]]))))
  }, integer(1))
  miss_cols <- names(miss_by_col)[miss_by_col > 0]
  
  dropped_info <- NULL
  if (n_dropped > 0) {
    dropped_idx <- which(!keep)
    dropped_ids <- as.character(df[[col_uid]][dropped_idx])
    treat_vals  <- df[[".treat_01"]][dropped_idx]
    role_vals   <- ifelse(treat_vals == 1, "Treatment", "Control")
    dropped_info <- data.frame(
      ID     = dropped_ids,
      Role   = role_vals,
      Reason = ifelse(!is_complete[dropped_idx],
                      "missing value(s)", "non-finite value(s)"),
      stringsAsFactors = FALSE
    )
    names(dropped_info)[1] <- col_uid
    errors <- c(errors, paste0(
      n_dropped, " unit(s) dropped due to missing or non-finite values in: ",
      paste(miss_cols, collapse = ", "), "."))
    df <- df[keep, , drop = FALSE]
    if (is_spat) x <- x[keep, ]
  }
  
  # -- Build formula using internal 0/1 treatment column --------------------
  frm <- as.formula(
    paste0(".treat_01 ~ ",
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
  
  # -- Remove internal .treat_01 column -------------------------------------
  # col_treatment with original values is already in matched_df (match.data()
  # returns all columns of the input df). Only the temporary .treat_01 needs
  # to be removed.
  matched_df[[".treat_01"]] <- NULL
  
  # -- Rename MatchIt-added columns to dot-prefix convention ----------------
  # match.data() adds: "distance", "weights", "subclass" (no dot prefix).
  # Rename all to dot-prefixed so users can distinguish app-added columns.
  rename_map <- c(distance = ".distance",
                  weights  = ".weights",
                  subclass = ".subclass")
  for (old_nm in names(rename_map)) {
    if (old_nm %in% names(matched_df)) {
      names(matched_df)[names(matched_df) == old_nm] <- rename_map[[old_nm]]
    }
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
  # terra::merge() joins a SpatVector with a plain data.frame on a shared
  # column, preserving geometry automatically. as.data.frame() is required to
  # strip the "matchdata" subclass from MatchIt's output, which would otherwise
  # cause dispatch to base::merge.data.frame and lose the geometry.
  if (is_spat) {
    out <- terra::merge(x[, col_uid], as.data.frame(matched_df_clean),
                        by = col_uid, all = FALSE)
    if (nrow(out) == 0) {
      stop("UID-based geometry merge produced 0 rows. ",
           "Check that '", col_uid, "' is present in both datasets.")
    }
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