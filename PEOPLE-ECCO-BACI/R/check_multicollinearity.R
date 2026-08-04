# =============================================================================
# check_multicollinearity.R
#
# Standalone function for assessing multicollinearity among covariates in a
# SpatVector.
#
# Usage inside the Shiny app: sourced automatically via global.R.
# Usage outside the app:
#   source("R/check_multicollinearity.R")
#   library(terra)
#
# Main entry point:
#   check_multicollinearity(x, vars)
# =============================================================================


# -----------------------------------------------------------------------------
# check_multicollinearity()
#
# Assesses multicollinearity among a set of covariates in a SpatVector.
# Categorical (non-numeric) variables are automatically excluded from all
# calculations, with a message listing which were dropped.
#
# Arguments:
#   x       SpatVector or data.frame. The dataset containing the variables.
#   vars    Character vector of variable names to assess.
#   use     Character. Method for handling missing values in cor(), passed
#           directly to cor(). Default "pairwise.complete.obs".
#
# Returns a named list:
#   $cor_matrix   Numeric matrix. Pearson correlation matrix for all numeric
#                 variables in vars. Diagonal is always 1.
#   $vif          Data frame with columns:
#                   Covariate  - variable name
#                   VIF        - variance inflation factor (rounded to 3 dp)
#                   Status     - "OK" (VIF < 5), "Moderate" (5-10), "Severe" (>= 10)
#   $vars_used    Character vector of variables actually used (numeric only).
#   $vars_dropped Character vector of variables excluded (non-numeric).
#   $n_complete   Integer. Number of rows with complete data across all vars_used.
#
# Notes:
#   - VIF is computed without external packages using the standard OLS
#     definition: for each variable j, regress it on all other variables and
#     compute VIF_j = 1 / (1 - R^2_j). Requires at least 2 numeric variables.
#   - A perfect linear relationship (R^2 = 1) returns VIF = Inf.
#   - Correlation requires at least 2 numeric variables.
#
# Examples:
#   v   <- terra::vect("covariates.gpkg")
#   res <- check_multicollinearity(v, c("ndvi", "elevation", "slope", "landuse"))
#   print(res$cor_matrix)
#   print(res$vif)
#   # landuse (factor/character) would be in res$vars_dropped
# -----------------------------------------------------------------------------
check_multicollinearity <- function(x, vars, use = "pairwise.complete.obs") {

  # -- Coerce input to data frame ---------------------------------------------
  if (inherits(x, "SpatVector")) {
    df <- as.data.frame(x)
  } else if (is.data.frame(x)) {
    df <- x
  } else {
    stop("x must be a SpatVector or data.frame.")
  }

  # -- Validate vars ----------------------------------------------------------
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    stop("Variables not found in x: ", paste(missing_vars, collapse = ", "))
  }
  if (length(vars) < 2) {
    stop("At least 2 variables are required.")
  }

  # -- Separate numeric from categorical --------------------------------------
  df_sub    <- df[, vars, drop = FALSE]
  is_num    <- vapply(df_sub, is.numeric, logical(1))
  vars_used <- vars[is_num]
  vars_drop <- vars[!is_num]

  if (length(vars_drop) > 0) {
    message("Excluding non-numeric variables from multicollinearity check: ",
            paste(vars_drop, collapse = ", "))
  }
  if (length(vars_used) < 2) {
    stop("At least 2 numeric variables are required. ",
         "All non-numeric variables were excluded.")
  }

  df_num <- df_sub[, vars_used, drop = FALSE]

  # -- Complete cases ---------------------------------------------------------
  complete_idx <- complete.cases(df_num)
  n_complete   <- sum(complete_idx)
  if (n_complete < 3) {
    stop("Fewer than 3 complete observations — cannot compute correlations.")
  }

  # -- Correlation matrix -----------------------------------------------------
  # Use the full df_num with pairwise.complete.obs (or user choice) so the
  # matrix is always symmetric with 1 on the diagonal.
  cor_mat <- cor(df_num, use = use)

  # -- VIF --------------------------------------------------------------------
  # Computed on complete cases only for consistency.
  df_complete <- df_num[complete_idx, , drop = FALSE]

  vif_vals <- vapply(seq_along(vars_used), function(j) {
    y_j  <- df_complete[[j]]
    x_j  <- df_complete[, -j, drop = FALSE]
    fit  <- lm(y_j ~ ., data = x_j)
    r2   <- summary(fit)$r.squared
    if (is.na(r2) || r2 >= 1) Inf else 1 / (1 - r2)
  }, numeric(1))

  status <- ifelse(vif_vals >= 10, "Severe",
            ifelse(vif_vals >= 5,  "Moderate", "OK"))

  vif_df <- data.frame(
    Covariate = vars_used,
    VIF       = round(vif_vals, 3),
    Status    = status,
    stringsAsFactors = FALSE
  )

  list(
    cor_matrix   = cor_mat,
    vif          = vif_df,
    vars_used    = vars_used,
    vars_dropped = vars_drop,
    n_complete   = n_complete
  )
}


# -----------------------------------------------------------------------------
# plot_cor_matrix()
#
# Plots the correlation matrix returned by check_multicollinearity() as a
# heatmap with correlation values printed in each cell. Intended for both
# standalone use and as the backend for renderPlot() in the Shiny app.
#
# Arguments:
#   cor_mat   Numeric matrix as returned in $cor_matrix.
#   cex_axis  Numeric. Font size multiplier for axis labels. Default 0.8.
#   cex_text  Numeric. Font size multiplier for cell values.  Default 0.75.
# -----------------------------------------------------------------------------
plot_cor_matrix <- function(cor_mat, cex_axis = 0.8, cex_text = 0.75) {

  n   <- ncol(cor_mat)
  nms <- colnames(cor_mat)

  # image() maps matrix[i,j] to position (x=i, y=j), with y increasing upward.
  # To display the matrix with row 1 at the top (conventional), reverse rows.
  # The axis labels and text coordinates must match this reversal consistently.
  display_mat <- cor_mat[nrow(cor_mat):1, ]   # flip rows, not columns

  old_par <- par(mar = c(max(nchar(nms)) * 0.6, max(nchar(nms)) * 0.6, 2, 1))
  on.exit(par(old_par))

  image(
    x    = seq_len(n),                 # x axis = columns
    y    = seq_len(n),                 # y axis = rows (reversed)
    z    = display_mat,
    col  = colorRampPalette(c("#c0392b", "white", "#2980b9"))(101),
    zlim = c(-1, 1),
    xaxt = "n", yaxt = "n",
    xlab = "", ylab = ""
  )

  # x axis: column names (bottom)
  axis(1, at = seq_len(n), labels = nms,      las = 2, cex.axis = cex_axis)
  # y axis: row names reversed so row 1 is at top
  axis(2, at = seq_len(n), labels = rev(nms), las = 1, cex.axis = cex_axis)

  # Cell labels: iterate over original cor_mat[row, col]
  # image y=1 corresponds to the LAST row of display_mat = first row of cor_mat
  for (col_idx in seq_len(n)) {
    for (row_idx in seq_len(n)) {
      val    <- cor_mat[row_idx, col_idx]
      # In display_mat, original row row_idx maps to y = (n + 1 - row_idx)
      y_pos  <- n + 1 - row_idx
      text(col_idx, y_pos,
           labels = formatC(round(val, 2), format = "f", digits = 2),
           cex    = cex_text,
           col    = if (abs(val) > 0.65) "white" else "black")
    }
  }
}
