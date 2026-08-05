# =============================================================================
# evaluate_matching.R
#
# Standalone functions for evaluating covariate balance after matching using
# the cobalt package.
#
# Usage inside the Shiny app: sourced automatically via global.R.
# Usage outside the app:
#   source("R/evaluate_matching.R")
#   library(cobalt)
#
# Main entry points:
#   get_balance_table(match_object, ...)
#   get_balance_plots(match_object, ...)
#   get_love_plot(match_object, ...)
#   evaluate_matching(match_object, ...)   # interactive wrapper for console use
# =============================================================================


# -----------------------------------------------------------------------------
# get_balance_table()
#
# Returns the balance table from cobalt::bal.tab() as a plain data frame,
# suitable for display in Shiny or printing to the console.
#
# Arguments:
#   match_object   A matchit object (output of MatchIt::matchit()).
#   binary         Character. How to treat binary variables: "std" for
#                  standardised mean difference, "raw" for raw difference.
#                  Default "std".
#   thresholds     Named numeric vector of balance thresholds passed to
#                  bal.tab(), e.g. c(m = 0.1). Default c(m = 0.1).
#   un             Logical. If TRUE (default), include unadjusted balance
#                  statistics (Diff.Un) alongside adjusted ones. Required to
#                  show pre-matching balance for comparison.
#   ...            Further arguments passed to cobalt::bal.tab().
#
# Returns a named list:
#   $bal_tab       The full cobalt bal.tab object (for further cobalt use).
#   $balance_df    data.frame of balance statistics, one row per covariate.
#   $sample_sizes  data.frame of sample sizes by group and matching status.
#   $method        Character. The matching method used.
# -----------------------------------------------------------------------------
get_balance_table <- function(match_object,
                              binary     = "std",
                              thresholds = c(m = 0.1),
                              un         = TRUE,
                              ...) {
  
  if (!inherits(match_object, "matchit")) {
    stop("match_object must be a matchit object.")
  }
  if (!requireNamespace("cobalt", quietly = TRUE)) {
    stop("Package 'cobalt' is required. Install with install.packages('cobalt').")
  }
  
  bt <- cobalt::bal.tab(match_object,
                        binary     = binary,
                        thresholds = thresholds,
                        un         = un,
                        ...)
  
  # Extract balance statistics data frame
  bal_df <- as.data.frame(bt$Balance)
  bal_df <- cbind(Covariate = rownames(bal_df), bal_df)
  rownames(bal_df) <- NULL
  
  # Round numeric columns for display
  num_cols <- vapply(bal_df, is.numeric, logical(1))
  bal_df[num_cols] <- lapply(bal_df[num_cols], round, digits = 3)
  
  # Sample sizes
  ss_df <- if (!is.null(bt$nn)) {
    as.data.frame(bt$nn)
  } else {
    NULL
  }
  
  list(
    bal_tab      = bt,
    balance_df   = bal_df,
    sample_sizes = ss_df,
    method       = match_object$info$method
  )
}


# -----------------------------------------------------------------------------
# get_balance_plots()
#
# Returns a named list of cobalt::bal.plot() ggplot objects, one per
# covariate. In Shiny these are rendered individually; in the console they
# can be printed in sequence.
#
# Arguments:
#   match_object   A matchit object.
#   which          Character. "both" (default) shows adjusted and unadjusted
#                  distributions side by side. "adjusted" or "unadjusted"
#                  shows one side only.
#   var_names      Character vector of covariate names to plot. NULL = all.
#   ...            Further arguments passed to cobalt::bal.plot().
#
# Returns a named list of ggplot objects (names = covariate names).
# Returns an empty list if the method does not support distributional plots
# (e.g. exact matching).
# -----------------------------------------------------------------------------
get_balance_plots <- function(match_object,
                              which     = "both",
                              var_names = NULL,
                              ...) {
  
  if (!requireNamespace("cobalt", quietly = TRUE)) {
    stop("Package 'cobalt' is required.")
  }
  
  # Methods where distributional overlap plots are not meaningful
  no_overlap_methods <- c("exact", "cem")
  method <- match_object$info$method
  if (!is.null(method) && method %in% no_overlap_methods) {
    message("Overlap plots are not produced for method = '", method, "'.")
    return(list())
  }
  
  # Determine variable names
  if (is.null(var_names)) {
    var_names <- names(match_object$X)
  }
  
  plots <- lapply(var_names, function(vn) {
    tryCatch(
      cobalt::bal.plot(match_object, var.name = vn, which = which, ...),
      error = function(e) {
        message("bal.plot failed for '", vn, "': ", conditionMessage(e))
        NULL
      }
    )
  })
  names(plots) <- var_names
  Filter(Negate(is.null), plots)
}


# -----------------------------------------------------------------------------
# get_love_plot()
#
# Returns a cobalt::love.plot() ggplot object showing standardised mean
# differences before and after matching for all covariates.
#
# Arguments:
#   match_object   A matchit object.
#   binary         Character. "std" (default) or "raw".
#   thresholds     Named numeric vector, e.g. c(m = 0.1). Default c(m = 0.1).
#   ...            Further arguments passed to cobalt::love.plot().
#
# Returns a ggplot object, or NULL if the method does not support it.
# -----------------------------------------------------------------------------
get_love_plot <- function(match_object,
                          binary     = "std",
                          thresholds = c(m = 0.1),
                          ...) {
  
  if (!requireNamespace("cobalt", quietly = TRUE)) {
    stop("Package 'cobalt' is required.")
  }
  
  method <- match_object$info$method
  
  # Love plot is not meaningful for exact matching (no distance or balance
  # statistic is computed — all covariates are exactly matched by definition)
  no_love_methods <- c("exact")
  if (!is.null(method) && method %in% no_love_methods) {
    message("Love plot is not produced for method = '", method, "'.")
    return(NULL)
  }
  
  tryCatch(
    cobalt::love.plot(match_object,
                      binary     = binary,
                      thresholds = thresholds,
                      ...),
    error = function(e) {
      message("love.plot failed: ", conditionMessage(e))
      NULL
    }
  )
}


# -----------------------------------------------------------------------------
# evaluate_matching()
#
# Interactive console wrapper. Prints the balance table, then (optionally)
# shows distributional overlap plots and a love plot one at a time, waiting
# for the user to press Enter between each.
#
# Arguments:
#   match_object       A matchit object.
#   covariate_overlap  Logical. Show bal.plot() per covariate? Default TRUE.
#   love_plot          Logical. Show love.plot()? Default TRUE.
#   binary             Character. Passed to bal.tab / love.plot. Default "std".
#   thresholds         Named numeric. Passed to bal.tab / love.plot.
#                      Default c(m = 0.1).
#   var_names          Character vector. Covariates to plot. NULL = all.
#
# Returns invisibly: a list with $balance, $overlap_plots, $love_plot.
# -----------------------------------------------------------------------------
evaluate_matching <- function(match_object,
                              covariate_overlap = TRUE,
                              love_plot         = TRUE,
                              binary            = "std",
                              thresholds        = c(m = 0.1),
                              var_names         = NULL) {
  
  # -- Balance table ----------------------------------------------------------
  cat("\n=== Balance table ===\n")
  bal <- get_balance_table(match_object,
                           binary     = binary,
                           thresholds = thresholds)
  print(bal$bal_tab)
  
  if (!is.null(bal$sample_sizes)) {
    cat("\nSample sizes:\n")
    print(bal$sample_sizes)
  }
  
  overlap_plots <- list()
  lp            <- NULL
  
  # -- Covariate overlap plots ------------------------------------------------
  if (isTRUE(covariate_overlap)) {
    overlap_plots <- get_balance_plots(match_object,
                                       which     = "both",
                                       var_names = var_names)
    for (nm in names(overlap_plots)) {
      cat("\n=== Overlap: ", nm, " ===\n", sep = "")
      print(overlap_plots[[nm]])
      readline(prompt = "Press <Enter> to continue.")
    }
  }
  
  # -- Love plot --------------------------------------------------------------
  if (isTRUE(love_plot)) {
    cat("\n=== Love plot ===\n")
    lp <- get_love_plot(match_object,
                        binary     = binary,
                        thresholds = thresholds)
    if (!is.null(lp)) {
      print(lp)
      readline(prompt = "Press <Enter> to continue.")
    }
  }
  
  invisible(list(
    balance       = bal,
    overlap_plots = overlap_plots,
    love_plot     = lp
  ))
}