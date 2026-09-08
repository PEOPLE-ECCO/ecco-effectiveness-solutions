# =============================================================================
# run_impact_assessment.R
#
# Standalone functions for BACI (Before-After Control-Impact) and
# CI (Control-Impact) impact assessment on matched spatial units.
#
# Usage inside the Shiny app: sourced automatically via global.R.
# Usage outside the app:
#   source("R/run_impact_assessment.R")
#   source("R/extract_covariates.R")   # needed for extract_local_raster etc.
#   library(terra); library(data.table)
#
# Main entry point:
#   run_impact_assessment(matched_vect, sources, col_treatment,
#                         col_uid, design, spatial_unit, ...)
# =============================================================================


# -----------------------------------------------------------------------------
# extract_impact_variable()
#
# Extracts one impact source (raster or vector file) for all units in the
# matched SpatVector, appending a suffix to distinguish before/after/effect.
#
# Arguments:
#   matched_vect   SpatVector. The matched control-impact units.
#   source_spec    Named list describing the source. Fields mirror those used
#                  in run_extraction() sources list, with the addition of:
#                    $slot   Character. "before", "after", or "effect" --
#                            appended as suffix to output column names.
#   col_uid        Character. Unique ID column name (for merge safety).
#
# Returns a data.frame with nrow(matched_vect) rows, columns named
# <layer>_<slot> (e.g. "ndvi_before", "ndvi_after").
# -----------------------------------------------------------------------------
extract_impact_variable <- function(matched_vect, source_spec, col_uid) {

  slot   <- source_spec$slot
  src_type <- source_spec$source_type

  if (src_type %in% c("Local raster", "URL raster", "PEOPLE-ECCO dataset")) {

    rast_path <- if (src_type == "URL raster") {
      paste0("/vsicurl/", source_spec$path)
    } else {
      source_spec$path
    }

    na_rm <- if (!is.null(source_spec$na.rm)) source_spec$na.rm else TRUE

    src_method   <- if (!is.null(source_spec$method)) { source_spec$method } else { "simple" }
    src_chunks   <- isTRUE(source_spec$use_chunks)
    src_chunksize <- if (!is.null(source_spec$chunk_size) && !is.na(source_spec$chunk_size)) {
      as.integer(source_spec$chunk_size)
    } else { 500L }

    # Extract with whatever prefix the filename gives, then strip it so
    # output columns are named by layer name only (e.g. "average", "trend").
    # This ensures check_variable_consistency() finds matching bases regardless
    # of the filename (e.g. "avgtrend_before.tif" vs "avgtrend_after.tif").
    df <- extract_local_raster(
      x            = rast_path,
      y            = matched_vect,
      fun          = source_spec$fun,
      method       = src_method,
      sel_lyr      = source_spec$sel_lyr,
      fun_fraction = source_spec$fun_fraction,
      fun_custom   = source_spec$fun_custom,
      layer_prefix = source_spec$layer_prefix,
      use_chunks   = src_chunks,
      chunk_size   = src_chunksize,
      na.rm        = na_rm
    )

    # Rename: strip the filename prefix so columns are named by layer name only.
    # extract_local_raster names columns <prefix>_<layername>; we want just
    # <layername> so the slot suffix produces <layername>_<slot>.
    rast_obj  <- terra::rast(rast_path)
    all_lyrs  <- names(rast_obj)
    # Determine which layers were actually extracted (respecting sel_lyr)
    sel       <- source_spec$sel_lyr
    if (!is.null(sel) && length(sel) > 0) {
      idx <- suppressWarnings(as.integer(sel))
      if (any(is.na(idx))) { idx <- match(sel, all_lyrs) }
      idx <- idx[!is.na(idx) & idx >= 1 & idx <= length(all_lyrs)]
      extracted_lyrs <- all_lyrs[idx]
    } else {
      extracted_lyrs <- all_lyrs
    }
    # Assign clean layer names (same number and order as df columns)
    if (length(extracted_lyrs) == ncol(df)) {
      names(df) <- make_colname(extracted_lyrs)
    }

  } else if (src_type == "Local vector") {

    vec_fun_val <- if (!is.null(source_spec$vec_fun)) { source_spec$vec_fun } else { "mean" }
    df <- extract_local_vector(
      x            = source_spec$path,
      y            = matched_vect,
      fun          = vec_fun_val,
      vec_attrs    = source_spec$vec_attrs,
      layer_prefix = source_spec$layer_prefix,
      na.rm        = TRUE
    )

  } else if (src_type == "from_vector") {

    # Values already in the matched_vect attributes
    attrs <- source_spec$attrs   # character vector of column names
    df_v  <- as.data.frame(matched_vect)
    missing_a <- setdiff(attrs, names(df_v))
    if (length(missing_a) > 0) {
      stop("Attribute(s) not found in matched vector: ",
           paste(missing_a, collapse = ", "))
    }
    df <- df_v[, attrs, drop = FALSE]

    # Apply rename_map if provided (after attrs paired by order, not by name)
    # rename_map: named vector where names=original col, values=target base name
    if (!is.null(source_spec$rename_map) && length(source_spec$rename_map) > 0) {
      for (orig_col in names(source_spec$rename_map)) {
        if (orig_col %in% names(df)) {
          new_base <- source_spec$rename_map[[orig_col]]
          names(df)[names(df) == orig_col] <- paste0(new_base, "_", slot)
        }
      }
      # Suffix already added above; skip the generic suffix addition below
      return(df)
    }

  } else {
    stop("Unknown source type: '", src_type, "'")
  }

  # Append slot suffix only to columns that don't already end with it.
  # This prevents double-suffixing when the user selects attributes that
  # already contain _before/_after in their name (e.g. "average_before").
  already_suffixed <- endsWith(names(df), paste0("_", slot))
  names(df)[!already_suffixed] <- paste0(names(df)[!already_suffixed], "_", slot)

  df
}


# -----------------------------------------------------------------------------
# check_variable_consistency()
#
# Checks that the "before" and "after" sources yield the same set of impact
# variable base names (before the _before/_after suffix), so that the
# effect = after - before subtraction is unambiguous.
#
# Arguments:
#   before_cols   Character vector of column names from the "before" extraction
#                 (with _before suffix already stripped or not).
#   after_cols    Character vector of column names from the "after" extraction.
#
# Returns invisibly TRUE on success; stops with an informative error if
# the variable sets do not match.
# -----------------------------------------------------------------------------
check_variable_consistency <- function(before_cols, after_cols) {
  # Strip all trailing _before/_after suffixes to get base variable names.
  # This handles both plain columns ("ndvi") and columns that already contain
  # the period as part of their name ("average_before", "trend_after") which
  # get double-suffixed by extract_impact_variable().
  strip_all <- function(x) {
    repeat {
      x_new <- sub("_(before|after)$", "", x)
      if (identical(x_new, x)) break
      x <- x_new
    }
    x
  }
  b <- sort(strip_all(before_cols))
  a <- sort(strip_all(after_cols))
  if (!identical(b, a)) {
    only_b <- setdiff(b, a)
    only_a <- setdiff(a, b)
    msg <- "Before and after sources yield different variable sets."
    if (length(only_b) > 0) {
      msg <- paste0(msg, " Only in before: ", paste(only_b, collapse = ", "), ".")
    }
    if (length(only_a) > 0) {
      msg <- paste0(msg, " Only in after: ", paste(only_a, collapse = ", "), ".")
    }
    stop(msg)
  }
  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# compute_baci()
#
# Core BACI contrast and p-value computation using data.table grouped
# operations for efficiency.
#
# For the individual case, units are grouped by their MatchIt subclass
# (column ".subclass" in the matched data). This avoids string-splitting
# .match_ids and secondary row lookups: each subclass already contains
# exactly the impact unit and its matched controls.
# Fallback to .match_ids grouping when .subclass is absent (e.g. some
# genetic matching configurations).
#
# Arguments:
#   dt             data.table. One row per matched unit, containing at minimum:
#                    col_uid       unique feature ID
#                    col_treatment 0/1 treatment indicator (1 = impact)
#                    ".subclass"   MatchIt subclass (preferred grouping key)
#                    col_match_ids comma-separated IDs of matched partners
#                                  (fallback grouping key)
#                    effect_cols   numeric columns to assess
#   col_uid        Character. Unique ID column.
#   col_treatment  Character. Treatment column (0 = control, 1 = impact).
#   col_match_ids  Character. Fallback column with matched partner IDs.
#   effect_cols    Character vector. Names of effect columns to assess.
#   spatial_unit   "individual" or "pooled".
#
# Returns a data.table:
#   individual: one row per impact unit with the uid column plus
#               <effect_col>_contrast and <effect_col>_pvalue columns.
#   pooled:     one row with <effect_col>_contrast and <effect_col>_pvalue.
# -----------------------------------------------------------------------------
compute_baci <- function(dt, col_uid, col_treatment, col_match_ids,
                         effect_cols, spatial_unit = "individual") {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }
  if (!data.table::is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  }

  # -- Pooled --─────────────────────────────────────────────────────────────────
  # Two-sample t-test comparing all impact vs all control values per variable.
  if (spatial_unit == "pooled") {
    results <- lapply(effect_cols, function(ec) {
      imp_vals  <- dt[get(col_treatment) == 1, get(ec)]
      ctrl_vals <- dt[get(col_treatment) == 0, get(ec)]
      imp_vals  <- imp_vals[is.finite(imp_vals)]
      ctrl_vals <- ctrl_vals[is.finite(ctrl_vals)]
      contrast  <- mean(ctrl_vals, na.rm = TRUE) - mean(imp_vals, na.rm = TRUE)
      pval <- tryCatch(
        t.test(ctrl_vals, imp_vals)$p.value,
        error = function(e) NA_real_
      )
      data.table::data.table(contrast = contrast, pvalue = pval)
    })
    out <- do.call(cbind, results)
    names(out) <- unlist(lapply(effect_cols, function(ec) {
      c(paste0(ec, "_contrast"), paste0(ec, "_pvalue"))
    }))
    return(out)
  }

  # -- Individual ---------------------------------------------------------------
  # The SpatVector stores impact-control linkage in .match_ids (comma-separated
  # UIDs) rather than MatchIt subclass rows, to avoid duplicate control rows in
  # the output geometry. Here we reconstruct a long-format data.table (one row
  # per impact-control pair, with a synthetic .grp column) purely for the
  # grouped t-test computation. The SpatVector format is unchanged.

  # Step 1: expand .match_ids into explicit pairs
  impact_dt  <- dt[get(col_treatment) == 1]
  control_dt <- dt[get(col_treatment) == 0]

  pair_list <- lapply(seq_len(nrow(impact_dt)), function(i) {
    imp_uid      <- as.character(impact_dt[[col_uid]][i])
    match_str    <- as.character(impact_dt[[col_match_ids]][i])
    partner_uids <- trimws(unlist(strsplit(match_str, ",")))
    partner_uids <- partner_uids[nchar(partner_uids) > 0]
    ctrl_rows    <- control_dt[as.character(get(col_uid)) %in% partner_uids]
    imp_row      <- impact_dt[i]
    if (nrow(ctrl_rows) == 0) return(NULL)
    imp_row[,   .grp := imp_uid]
    ctrl_rows[, .grp := imp_uid]
    data.table::rbindlist(list(imp_row, ctrl_rows), fill = TRUE)
  })
  pair_dt <- data.table::rbindlist(Filter(Negate(is.null), pair_list),
                                   fill = TRUE)

  if (nrow(pair_dt) == 0) {
    stop("Could not reconstruct pairs from .match_ids. ",
         "Check that the matched vector contains a valid .match_ids column.")
  }

  # Step 2: grouped computation using data.table by=.grp
  # Reference treatment and effect columns by name inside the expression
  # to avoid any positional ambiguity with .SD[[1]]/.SD[[2]] that could
  # scramble results when column order varies.

  all_results <- lapply(effect_cols, function(ec) {
    # Capture column names as local variables for use inside data.table expr
    treat_col_local <- col_treatment
    ec_col_local    <- ec
    result <- pair_dt[
      !is.na(.grp),
      {
        treat_vec <- get(treat_col_local)
        ec_vec    <- get(ec_col_local)
        ctrl_val  <- ec_vec[is.finite(ec_vec) & treat_vec == 0]
        imp_val   <- ec_vec[is.finite(ec_vec) & treat_vec == 1]
        contrast  <- mean(ctrl_val, na.rm = TRUE) - mean(imp_val, na.rm = TRUE)
        pval <- if (length(ctrl_val) == 0 || length(imp_val) == 0) {
          NA_real_
        } else if (length(ctrl_val) == 1) {
          NA_real_   # 1:1 - single control, no variance for t-test
        } else {
          # 1:k - paired differences test: H0 mean(ctrl - impact) == 0
          tryCatch({
            pv <- t.test(ctrl_val - imp_val[1])$p.value
            if (is.nan(pv)) NA_real_ else pv
          }, error = function(e) NA_real_)
        }
        list(.contrast = contrast, .pvalue = pval)
      },
      by = ".grp"
    ]
    data.table::setnames(result,
      c(".contrast", ".pvalue"),
      c(paste0(ec, "_contrast"), paste0(ec, "_pvalue")))
    result
  })

  # Step 3: merge results; .grp == impact unit UID
  out <- Reduce(function(a, b) merge(a, b, by = ".grp", all = TRUE),
                all_results)
  data.table::setnames(out, ".grp", col_uid)
  out
}


# -----------------------------------------------------------------------------
# run_impact_assessment()
#
# Orchestrates the full impact assessment pipeline.
#
# Arguments:
#   matched_vect   SpatVector. Matched control-impact units (output of
#                  run_matching(), or loaded from file).
#   sources        Named list with elements "before", "after", and/or "effect",
#                  each a source_spec list as described in extract_impact_variable().
#                  For BACI design supply "before" and "after".
#                  For CI design supply "effect".
#                  "from_vector" sources have $source_type = "from_vector" and
#                  $attrs = character vector of attribute names.
#   col_treatment  Character. Treatment column name (1 = impact, 0 = control).
#   col_uid        Character. Unique ID column name.
#   col_match_ids  Character. Column with comma-separated matched partner IDs.
#                  Default ".match_ids".
#   design         "baci" (before-after) or "ci" (effect only). Default "baci".
#   spatial_unit   "individual" or "pooled". Default "individual".
#   progress_fun   Optional function(message, fraction) for progress reporting.
#
# Returns a named list:
#   $result_vect   SpatVector. Impact units only (treatment == 1) with
#                  appended effect and BACI contrast/p-value columns.
#                  For pooled analysis, a single-row data.frame is returned
#                  instead (no geometry).
#   $result_df     data.frame version of the above (for tabular display).
#   $effect_cols   Character vector of effect column names used.
#   $errors        Character vector of non-fatal error messages.
# -----------------------------------------------------------------------------
run_impact_assessment <- function(matched_vect,
                                  sources,
                                  col_treatment,
                                  col_uid,
                                  treat_value    = NULL,
                                  col_match_ids  = ".match_ids",
                                  design         = "baci",
                                  spatial_unit   = "individual",
                                  progress_fun   = NULL) {

  errors <- character(0)
  n_steps <- 4
  report  <- function(msg, step) {
    if (!is.null(progress_fun)) progress_fun(msg, step / n_steps)
  }

  # -- Step 1: extract impact variables from external sources ---------------
  report("Extracting impact variables...", 1)

  full_df  <- as.data.frame(matched_vect)

  # Ensure col_uid is a valid non-empty column name present in the data.
  # Priority: user-supplied col_uid > ".uid" auto-generated by run_matching >
  # sequential row index as last resort.
  if (is.null(col_uid) || nchar(col_uid) == 0 || !col_uid %in% names(full_df)) {
    if (".uid" %in% names(full_df)) {
      col_uid <- ".uid"
    } else {
      full_df[[".row_uid"]] <- as.character(seq_len(nrow(full_df)))
      col_uid <- ".row_uid"
    }
  }

  # Coerce treatment column to 0/1 using treat_value if supplied
  if (!is.null(treat_value) && nchar(as.character(treat_value)) > 0) {
    tv <- as.character(treat_value)
    full_df[[col_treatment]] <- as.integer(
      as.character(full_df[[col_treatment]]) == tv)
  }

  # base_df carries only the key columns needed for matching/grouping.
  # Extracted impact variables are added separately so original attributes
  # with similar names (e.g. trend_before already in the vector) do not
  # interfere with the effect computation.
  key_cols <- unique(c(col_uid, col_treatment, col_match_ids))
  key_cols <- intersect(key_cols, names(full_df))
  base_df  <- full_df[, key_cols, drop = FALSE]

  append_extracted <- function(slot_name) {
    src <- sources[[slot_name]]
    if (is.null(src)) return(NULL)
    src$slot <- slot_name
    tryCatch(
      extract_impact_variable(matched_vect, src, col_uid),
      error = function(e) {
        errors <<- c(errors, paste0("[", slot_name, "] ", conditionMessage(e)))
        NULL
      }
    )
  }

  if (design == "baci") {
    before_df <- append_extracted("before")
    after_df  <- append_extracted("after")
    if (!is.null(before_df) && !is.null(after_df)) {
      tryCatch(
        check_variable_consistency(names(before_df), names(after_df)),
        error = function(e) { errors <<- c(errors, conditionMessage(e)) }
      )
    }
    if (!is.null(before_df)) base_df <- cbind(base_df, before_df)
    if (!is.null(after_df))  base_df <- cbind(base_df, after_df)

  } else {
    effect_df <- append_extracted("effect")
    if (!is.null(effect_df)) base_df <- cbind(base_df, effect_df)
  }

  # -- Step 2: compute effect = after - before (BACI design) ----------------
  report("Computing effect variables...", 2)

  if (design == "baci") {
    before_cols <- grep("_before$", names(base_df), value = TRUE)
    after_cols  <- grep("_after$",  names(base_df), value = TRUE)
    # Strip ALL trailing _before/_after to get true base names,
    # then pair by matching base name (or by order if names differ)
    strip_all <- function(x, sfx) {
      repeat {
        x_new <- sub(paste0("_", sfx, "$"), "", x)
        if (identical(x_new, x)) break
        x <- x_new
      }
      x
    }
    b_bases <- strip_all(before_cols, "before")
    a_bases <- strip_all(after_cols,  "after")
    # Pair: try to match by base name; fall back to positional pairing
    if (identical(sort(b_bases), sort(a_bases))) {
      # Reorder after_cols to match before_cols order by base name
      a_order <- match(b_bases, a_bases)
      after_cols_ordered <- after_cols[a_order]
    } else {
      # Positional pairing (names don't match; user was warned in UI)
      after_cols_ordered <- after_cols[seq_along(before_cols)]
    }
    for (k in seq_along(before_cols)) {
      bc <- before_cols[k]
      ac <- after_cols_ordered[k]
      if (!is.na(ac) && bc %in% names(base_df) && ac %in% names(base_df)) {
        base_name <- b_bases[k]
        base_df[[paste0(base_name, "_effect")]] <- base_df[[ac]] - base_df[[bc]]
      }
    }
    effect_cols <- paste0(b_bases, "_effect")
    effect_cols <- effect_cols[effect_cols %in% names(base_df)]
  } else {
    effect_cols <- grep("_effect$", names(base_df), value = TRUE)
  }

  if (length(effect_cols) == 0) {
    errors <- c(errors, "No effect columns could be derived. Check source inputs.")
    return(list(result_vect = NULL, result_df = NULL,
                effect_cols = character(0), errors = errors))
  }

  # -- Step 3: compute BACI contrast and p-value ----------------------------
  report("Computing BACI contrast and p-value...", 3)

  dt <- data.table::as.data.table(base_df)

  baci_results <- tryCatch(
    compute_baci(dt, col_uid, col_treatment, col_match_ids,
                 effect_cols, spatial_unit),
    error = function(e) {
      errors <<- c(errors, paste0("BACI computation failed: ", conditionMessage(e)))
      NULL
    }
  )

  # -- Step 4: assemble output ----------------------------------------------
  report("Assembling output...", 4)

  if (spatial_unit == "pooled" || is.null(baci_results)) {
    # Pooled: return a plain data.frame (no per-unit geometry)
    out_df <- if (!is.null(baci_results)) as.data.frame(baci_results) else NULL
    return(list(result_vect = NULL, result_df = out_df,
                effect_cols = effect_cols, errors = errors))
  }

  # Individual: attach new columns to impact units using terra::merge() which
  # handles geometry alignment automatically via the UID key.
  # This avoids any manual row-order matching that can silently scramble results.

  # Extract impact unit SpatVector (geometry preserved from matched_vect)
  impact_idx  <- which(full_df[[col_treatment]] == 1)
  geom_impact <- matched_vect[impact_idx, ]

  # Ensure col_uid is present in geom_impact
  if (!col_uid %in% names(geom_impact)) {
    geom_impact[[col_uid]] <- as.character(seq_len(nrow(geom_impact)))
  }

  # Build attribute data.frame with new columns to add:
  # before/after/effect from base_df + BACI contrast/pvalue from baci_results
  baci_df  <- as.data.frame(baci_results)
  base_imp <- base_df[base_df[[col_treatment]] == 1, , drop = FALSE]

  # New columns: those in base_df that are not already in matched_vect
  existing_cols <- names(as.data.frame(matched_vect))
  new_base_cols <- setdiff(names(base_imp), c(existing_cols, col_treatment))
  new_baci_cols <- setdiff(names(baci_df), c(col_uid, existing_cols))

  # Assemble attribute table: uid + new base cols + baci cols
  add_df <- base_imp[, c(col_uid, new_base_cols), drop = FALSE]
  # Merge baci results onto add_df by uid
  add_df <- merge(add_df,
                  baci_df[, c(col_uid, new_baci_cols), drop = FALSE],
                  by = col_uid, all.x = TRUE, sort = FALSE)

  # Sanitise
  for (cn in names(add_df)) {
    if (is.factor(add_df[[cn]])) add_df[[cn]] <- as.character(add_df[[cn]])
  }

  # Use terra::merge to join attribute table onto geometry via UID
  # terra::merge preserves geometry and aligns by key -- no manual row matching
  out_vect <- terra::merge(geom_impact, as.data.frame(add_df),
                           by = col_uid, all.x = TRUE)

  list(
    result_vect = out_vect,
    result_df   = as.data.frame(out_vect),
    effect_cols = effect_cols,
    errors      = errors
  )
}
