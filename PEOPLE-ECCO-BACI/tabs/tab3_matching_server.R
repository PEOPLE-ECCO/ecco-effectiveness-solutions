# =============================================================================
# tab3_matching_server.R
# Server logic for Tab 3: Matching analysis
# Sourced inside server() with local=TRUE - has access to input/output/session

  # -- Tab 4: Matching input -------------------------------------------------

  # Reactive: resolve the matching SpatVector from either Tab 3 output
  # or a user-supplied file path.
  match_vect_data <- reactive({
    if (input$match_input_source == "from_tab") {
      res <- extraction_result()
      if (is.null(res) || is.null(res$result)) return(NULL)
      v <- res$result
      if (!inherits(v, "SpatVector")) return(NULL)
      v
    } else {
      if (is.null(input$match_vect_file)) return(NULL)
      v <- match_vect_from_file()
      if (!is.null(v) && !inherits(v, "SpatVector")) return(NULL)
      v
    }
  })

  # Reactive for the file-upload path (mirrors Tab 2 pattern)
  # Cache the loaded SpatVector in a reactiveVal so file.rename() only runs
  # once. A reactive re-evaluates on every read; file.rename() on an already-
  # renamed file fails silently and returns a bad path to terra::vect().
  match_vect_file_cache <- reactiveVal(NULL)

  observeEvent(input$match_vect_file, {
    req(input$match_vect_file)
    paths    <- input$match_vect_file$datapath
    names_up <- input$match_vect_file$name
    new_paths <- file.path(dirname(paths), names_up)
    file.rename(paths, new_paths)
    is_shp <- endsWith(tolower(names_up), ".shp")
    entry  <- if (any(is_shp)) { new_paths[is_shp] } else { new_paths[1] }
    result <- tryCatch(
      terra::vect(entry),
      error = function(e) {
        showNotification(paste0("Could not load vector file: ",
                                conditionMessage(e)), type = "error")
        NULL
      }
    )
    match_vect_file_cache(result)
  }, ignoreNULL = TRUE)

  match_vect_from_file <- reactive({
    match_vect_file_cache()
  })

  # Status / metadata UI
  output$match_vect_status_ui <- renderUI({
    if (input$match_input_source == "from_tab") {
      res <- extraction_result()
      if (is.null(res) || is.null(res$result)) {
        return(p(class = "placeholder-msg",
                 "No extraction result yet. Run extraction in the Extract matching
                  covariates tab first."))
      }
      v <- res$result
      div(class = "info-row", style = "margin-top:8px;",
        span(class = "info-pill", paste0(nrow(v), " features")),
        span(class = "info-pill", paste0(ncol(as.data.frame(v)), " attributes")),
        span(class = "info-pill", "From Tab 3")
      )
    } else {
      if (is.null(input$match_vect_file)) return(NULL)
      v <- match_vect_from_file()
      if (is.null(v)) {
        return(p(style = "color:#c0392b; font-size:12px; margin-top:8px;",
                 "! Could not read file. Please check it is a valid vector file."))
      }
      crs_name <- terra::crs(v, describe = TRUE)$name
      div(class = "info-row", style = "margin-top:8px;",
        span(class = "info-pill", paste0(nrow(v), " features")),
        span(class = "info-pill", paste0(ncol(as.data.frame(v)), " attributes")),
        if (!is.na(crs_name) && nchar(crs_name) > 0) {
          span(class = "info-pill", crs_name)
        } else {
          NULL
        }
      )
    }
  })

  # Attribute selector for plot
  # Plot card: attr selector at top, plot below
  output$match_plot_card_ui <- renderUI({
    v <- match_vect_data()
    if (is.null(v)) return(NULL)
    attr_names <- names(v)
    div(class = "card", style = "height:100%;",
      div(class = "card-title",
        span(class = "icon", "\U0001f5fa"),
        "Attribute preview"
      ),
      if (length(attr_names) > 0) {
        selectInput("match_plot_attr",
          label    = "Attribute to visualise",
          choices  = setNames(attr_names, attr_names),
          selected = attr_names[1],
          width    = "100%")
      } else { NULL },
      plotOutput("match_vect_plot", height = "480px")
    )
  })

  output$match_vect_plot <- renderPlot({
    v    <- match_vect_data()
    req(!is.null(v) && !is.character(v))
    attr <- input$match_plot_attr
    if (!is.null(attr) && attr %in% names(v)) {
      terra::plot(v, attr, main = attr)
    } else {
      terra::plot(v)
    }
  }, res = 96, bg = "white")

  # -- Unique ID selector ----------------------------------------------------
  output$match_uid_ui <- renderUI({
    v <- match_vect_data()
    if (is.null(v)) return(NULL)
    attr_names <- names(v)
    # Pre-fill from Tab 2 uid_field if available
    pre_uid <- if (isTRUE(input$use_uid) &&
                   !is.null(input$uid_field) &&
                   input$uid_field %in% attr_names) {
      input$uid_field
    } else {
      ""
    }
    tagList(
      selectInput("match_uid_field",
        label    = "Unique feature ID (optional)",
        choices  = c("None (auto-generate)" = "", setNames(attr_names, attr_names)),
        selected = pre_uid,
        width    = "100%"),
      uiOutput("match_uid_warning_ui")
    )
  })

  output$match_uid_warning_ui <- renderUI({
    uid <- input$match_uid_field
    if (is.null(uid) || nchar(uid) == 0) return(NULL)
    v <- match_vect_data()
    if (is.null(v)) return(NULL)
    vals <- as.data.frame(v)[[uid]]
    if (anyDuplicated(vals) > 0) {
      p(style = "color:#c0392b; font-size:12px; margin:2px 0 0 0;",
        paste0("! Column '", uid, "' has duplicate values and cannot be ",
               "used as a unique ID."))
    } else {
      NULL
    }
  })

  # -- Treatment attribute selector ------------------------------------------
  # Pre-fills from Tab 2 ci_field when carrying over from extraction result
  output$match_treatment_ui <- renderUI({
    v <- match_vect_data()
    if (is.null(v)) return(NULL)
    attr_names <- names(v)
    # Pre-select Tab 2 treatment field if it exists in this dataset
    pre_select <- if (!is.null(input$ci_field) && input$ci_field %in% attr_names) {
      input$ci_field
    } else {
      attr_names[1]
    }
    tagList(
      selectInput("match_treatment",
        label    = "Treatment attribute",
        choices  = setNames(attr_names, attr_names),
        selected = pre_select,
        width    = "100%"),
      uiOutput("match_treat_value_ui")
    )
  })

  output$match_treat_value_ui <- renderUI({
    v     <- match_vect_data()
    treat <- input$match_treatment
    if (is.null(v) || is.null(treat) || !treat %in% names(v)) return(NULL)
    vals  <- sort(unique(na.omit(as.data.frame(v)[[treat]])))
    if (length(vals) < 2) return(NULL)
    # Default: if values are 0/1, pre-select 1; otherwise last (highest) value
    default_val <- if (all(vals %in% c(0, 1))) { "1" } else { as.character(vals[length(vals)]) }
    selectInput("match_treat_value",
      label    = "Value indicating treatment (1 = treated)",
      choices  = setNames(as.character(vals), as.character(vals)),
      selected = default_val,
      width    = "100%")
  })

  # -- Covariate selector -----------------------------------------------------
  # Pre-fills from Tab 3 col_matchvars selection when carrying over
  output$match_covars_ui <- renderUI({
    v <- match_vect_data()
    if (is.null(v)) return(NULL)
    attr_names  <- names(v)
    # Remove treatment and UID fields from covariate options
    treat      <- input$match_treatment
    uid        <- input$match_uid_field
    excl       <- c(treat, if (!is.null(uid) && nchar(uid) > 0) uid else NULL)
    covar_opts <- setdiff(attr_names, excl)
    # Pre-select Tab 3 card-1 covariates that exist in this dataset
    tab3_sel    <- input$col_matchvars
    pre_select  <- if (!is.null(tab3_sel) && length(tab3_sel) > 0) {
      intersect(tab3_sel, covar_opts)
    } else {
      covar_opts
    }
    tagList(
      selectInput("match_covars",
        label    = "Matching covariates",
        choices  = setNames(covar_opts, covar_opts),
        selected = pre_select,
        multiple = TRUE,
        width    = "100%"),
      uiOutput("match_covars_warning_ui")
    )
  })

  output$match_covars_warning_ui <- renderUI({
    covars <- input$match_covars
    if (is.null(covars) || length(covars) == 0) {
      p(style = "color:#c0392b; font-size:12px; margin:4px 0 0 0;",
        "! At least one covariate must be selected.")
    } else {
      NULL
    }
  })

  # -- Multicollinearity checkbox ---------------------------------------------
  output$match_multicol_check_ui <- renderUI({
    v <- match_vect_data()
    if (is.null(v)) return(NULL)
    div(style = "margin-top:8px; display:flex; align-items:center; gap:10px;",
      checkboxInput("check_multicol",
        label = "Check multicollinearity of selected covariates",
        value = FALSE)
    )
  })

  # -- Multicollinearity output (correlation matrix + VIF) -------------------
  # Reactive: run check_multicollinearity() whenever covariates change
  multicol_result <- reactive({
    req(input$check_multicol, match_vect_data(), input$match_covars)
    v      <- match_vect_data()
    covars <- input$match_covars
    if (length(covars) < 2) return(NULL)
    tryCatch(
      check_multicollinearity(v, covars),
      error = function(e) { list(error = conditionMessage(e)) }
    )
  })

  output$match_multicol_output_ui <- renderUI({
    if (!isTRUE(input$check_multicol)) return(NULL)
    if (is.null(match_vect_data())) return(NULL)

    res <- multicol_result()

    if (is.null(res)) {
      return(div(class = "card",
        div(class = "card-title", "Multicollinearity check"),
        p(style = "color:#e67e22; font-size:13px;",
          "At least 2 numeric covariates must be selected.")
      ))
    }

    if (!is.null(res$error)) {
      return(div(class = "card",
        div(class = "card-title", "Multicollinearity check"),
        p(style = "color:#c0392b; font-size:13px;", paste0("! ", res$error))
      ))
    }

    # Note if any variables were dropped
    drop_note <- if (length(res$vars_dropped) > 0) {
      p(style = "font-size:12px; color:#e67e22; margin-bottom:8px;",
        paste0("Note: excluded non-numeric variables: ",
               paste(res$vars_dropped, collapse = ", ")))
    } else {
      NULL
    }

    div(class = "card",
      div(class = "card-title",
        span(class = "icon", "\U0001f4ca"),
        "Multicollinearity check"
      ),
      p(style = "font-size:13px; color:#666; margin-bottom:4px;",
        paste0(res$n_complete, " complete observations, ",
               length(res$vars_used), " numeric covariates.")),
      drop_note,
      strong(style = "font-size:13px;", "Correlation matrix"),
      plotOutput("match_corr_plot",
        height = paste0(max(300, min(800, 60 * length(res$vars_used) + 120)), "px")),
      div(class = "section-divider"),
      strong(style = "font-size:13px;", "Variance Inflation Factor (VIF)"),
      p(style = "font-size:12px; color:#888; margin:4px 0 8px 0;",
        "VIF >= 5: moderate multicollinearity; VIF >= 10: severe."),
      tableOutput("match_vif_table")
    )
  })

  output$match_corr_plot <- renderPlot({
    res <- multicol_result()
    req(!is.null(res) && is.null(res$error))
    plot_cor_matrix(res$cor_matrix)
  }, res = 96, bg = "white",
  height = function() {
    res <- multicol_result()
    if (is.null(res) || is.null(res$cor_matrix)) return(400)
    n <- ncol(res$cor_matrix)
    max(300, min(800, 60 * n + 120))   # same formula as plotOutput above
  })

  output$match_vif_table <- renderTable({
    res <- multicol_result()
    req(!is.null(res) && is.null(res$error))
    res$vif
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 3)

  # =========================================================================
  # -- Matching parameters: conditional renderUI blocks ----------------------
  # Ordering: most commonly varied parameters first, advanced options last.
  # =========================================================================

  # Helper: available attribute names for variable selectors
  mi_all_attr <- reactive({
    v <- match_vect_data()
    if (is.null(v)) { character(0) } else { names(v) }
  })

  # Selected covariate names
  mi_covar_names <- reactive({
    if (is.null(input$match_covars)) { character(0) } else { input$match_covars }
  })

  # Methods that use a distance measure
  mi_uses_distance <- reactive({
    !input$mi_method %in% c("exact", "cem", "cardinality")
  })

  # Distance is a propensity score method (not a pure distance matrix or none)
  mi_ps_distance <- reactive({
    d <- input$mi_distance
    mi_uses_distance() &&
      !is.null(d) &&
      !d %in% c("mahalanobis", "robust_mahalanobis",
                 "scaled_euclidean", "euclidean", "none")
  })

  # -- 1. Distance ----------------------------------------------------------
  # Only renders the distance dropdown itself; link and mahvars are in
  # separate outputs so changing distance does not reset this input.
  output$mi_distance_ui <- renderUI({
    if (!mi_uses_distance()) return(NULL)
    tagList(
      selectInput("mi_distance", label = "distance",
        choices = c(
          "glm (propensity score)"   = "glm",
          "mahalanobis"              = "mahalanobis",
          "robust_mahalanobis"       = "robust_mahalanobis",
          "scaled_euclidean"         = "scaled_euclidean",
          "euclidean"                = "euclidean",
          "none"                     = "none"),
        selected = "glm", width = "100%"),
      uiOutput("mi_link_ui")
    )
  })

  output$mi_link_ui <- renderUI({
    if (!mi_ps_distance()) return(NULL)
    selectInput("mi_link", label = "link (PS model link function)",
      choices  = c("logit","probit","log","cloglog","cauchit"),
      selected = "logit", width = "100%")
  })

  # -- 2. replace + ratio ---------------------------------------------------
  output$mi_replace_ratio_ui <- renderUI({
    method       <- input$mi_method
    show_replace <- method %in% c("nearest","optimal","genetic")
    show_ratio   <- method %in% c("nearest","optimal","genetic","cardinality")
    if (!show_replace && !show_ratio) return(NULL)
    tagList(
      if (show_replace) {
        selectInput("mi_replace", label = "replace (matching with replacement)",
          choices = c("FALSE","TRUE"), selected = "FALSE", width = "100%")
      } else { NULL },
      if (show_ratio) {
        numericInput("mi_ratio", label = "ratio (k controls per treated unit)",
          value = 1, min = 1, step = 1, width = "100%")
      } else { NULL }
    )
  })

  # -- 3. Caliper -----------------------------------------------------------
  # Caliper values are read directly in click_matching with isolate().

  output$mi_caliper_ui <- renderUI({
    if (!input$mi_method %in% c("nearest","optimal","genetic","full","quick")) {
      return(NULL)
    }
    covars       <- input$match_covars
    if (is.null(covars) || length(covars) == 0) return(NULL)
    show_overall <- mi_uses_distance()

    # Build one row: label | numericInput | SD/raw selector
    make_row <- function(v, label) {
      safe_id <- gsub("[^a-zA-Z0-9_]", "_", v)
      val_id  <- paste0("mi_cal_val_", safe_id)
      std_id  <- paste0("mi_cal_std_", safe_id)
      div(style = "display:flex; align-items:center; gap:8px; margin-bottom:4px;",
        div(style = "flex:2; font-size:12px; color:#444; padding-top:8px;", label),
        div(style = "flex:1;",
          numericInput(val_id, label = NULL,
            value = NA, min = 0, step = 0.1, width = "100%")
        ),
        div(style = "flex:1;",
          selectInput(std_id, label = NULL,
            choices  = c("SD units" = "TRUE", "Raw units" = "FALSE"),
            selected = "TRUE", width = "100%")
        )
      )
    }

    tagList(
      # Title matching other parameter labels
      tags$label(style = "font-weight:600; font-size:13px;", "caliper"),
      # Column headers
      div(style = "display:flex; gap:8px; margin-top:4px; margin-bottom:2px;",
        div(style = "flex:2;"),
        div(style = "flex:1; font-size:11px; color:#888; text-align:center;",
          "Value"),
        div(style = "flex:1; font-size:11px; color:#888; text-align:center;",
          "Units")
      ),
      # Overall distance row: only shown when method uses a distance metric
      if (show_overall) make_row(".distance", "Overall distance") else NULL,
      if (show_overall) {
        div(class = "section-divider", style = "margin:6px 0;")
      } else { NULL },
      # Per-variable calipers: collapsed by default, triangle indicator from
      # browser's native <details> disclosure widget
      tags$details(
        tags$summary(
          style = "font-size:12px; color:#555; cursor:pointer; margin-bottom:6px;
                   user-select:none; list-style:disclosure-closed;",
          "Per-variable calipers"
        ),
        tagList(lapply(seq_along(covars), function(i) {
          make_row(covars[i], covars[i])
        }))
      ),
      p(style = "font-size:11px; color:#888; margin-top:6px;",
        "Leave blank for no caliper. SD units = caliper in standard deviations ",
        "of the variable (recommended). Raw units = original variable units.")
    )
  })

  # -- 4. m.order (nearest only) --------------------------------------------
  output$mi_morder_ui <- renderUI({
    if (input$mi_method != "nearest") return(NULL)
    choices <- if (mi_ps_distance()) {
      c("largest (default for PS)" = "largest",
        "smallest"                 = "smallest",
        "random"                   = "random",
        "data"                     = "data")
    } else {
      c("data (default)"           = "data",
        "largest"                  = "largest",
        "smallest"                 = "smallest",
        "random"                   = "random")
    }
    selectInput("mi_m_order", label = "m.order (matching order)",
      choices = choices, selected = names(choices)[1], width = "100%")
  })

  # -- 5. discard + reestimate (PS distance only) ---------------------------
  # reestimate is in a separate output so changing discard does not reset it.
  output$mi_discard_ui <- renderUI({
    if (!mi_ps_distance()) return(NULL)
    tagList(
      selectInput("mi_discard",
        label    = "discard (common support restriction)",
        choices  = c("none","treated","control","both"),
        selected = "none", width = "100%"),
      uiOutput("mi_reestimate_ui")
    )
  })

  output$mi_reestimate_ui <- renderUI({
    d <- input$mi_discard
    if (is.null(d) || d == "none") return(NULL)
    selectInput("mi_reestimate",
      label    = "reestimate (re-estimate PS after discarding?)",
      choices  = c("FALSE","TRUE"), selected = "FALSE", width = "100%")
  })

  # -- 6. exact + antiexact -------------------------------------------------
  output$mi_exact_ui <- renderUI({
    method <- input$mi_method
    covars <- mi_covar_names()
    show_exact     <- method %in% c("nearest","optimal","full","quick","genetic","cem")
    show_antiexact <- method %in% c("nearest","optimal","genetic")
    if (!show_exact && !show_antiexact) return(NULL)
    tagList(
      if (show_exact && length(covars) > 0) {
        selectInput("mi_exact",
          label    = "exact (force exact match on these variables)",
          choices  = covars, selected = NULL, multiple = TRUE, width = "100%")
      } else { NULL },
      if (show_antiexact && length(covars) > 0) {
        selectInput("mi_antiexact",
          label    = "antiexact (paired units must differ on these variables)",
          choices  = covars, selected = NULL, multiple = TRUE, width = "100%")
      } else { NULL }
    )
  })

  # -- 7. mahvars (PS distance only) ----------------------------------------
  # Shown separately from distance group so it appears after exact/antiexact
  # in the parameter flow (users set exact constraints before mahvars).
  # Kept in its own output so it can be positioned correctly in the card.

  # -- 8. Method-specific extras --------------------------------------------
  output$mi_method_extras_ui <- renderUI({
    method <- input$mi_method
    if (method == "subclass") {
      numericInput("mi_subclass",
        label = "subclass (number of subclasses)",
        value = 6, min = 2, step = 1, width = "100%")
    } else if (method == "cem") {
      selectInput("mi_k2k",
        label   = "k2k (prune to equal-sized groups after CEM?)",
        choices = c("FALSE","TRUE"), selected = "FALSE", width = "100%")
    } else {
      NULL
    }
  })

  # -- 9. s.weights ---------------------------------------------------------
  output$mi_sweights_ui <- renderUI({
    attrs <- mi_all_attr()
    if (length(attrs) == 0) return(NULL)
    tagList(
      div(class = "section-divider"),
      p(style = "font-size:12px; color:#888; margin-bottom:4px;",
        "Advanced options"),
      selectInput("mi_sweights",
        label    = "s.weights (sampling weight attribute, optional)",
        choices  = c("None" = "", attrs),
        selected = "",
        width    = "100%")
    )
  })

  # -- 10. mahvars (PS only, advanced) --------------------------------------
  output$mi_mahvars_ui <- renderUI({
    if (!mi_ps_distance()) return(NULL)
    covars <- mi_covar_names()
    if (length(covars) == 0) return(NULL)
    selectInput("mi_mahvars",
      label    = "mahvars (Mahalanobis matching within PS caliper)",
      choices  = covars, selected = NULL, multiple = TRUE, width = "100%")
  })

  # -- 11. distance.options (advanced) --------------------------------------
  output$mi_dist_options_ui <- renderUI({
    if (!mi_uses_distance()) return(NULL)
    textInput("mi_distance_options",
      label       = "distance.options (named R list as text, optional)",
      placeholder = 'e.g. list(family = binomial(link = "probit"))',
      width       = "100%")
  })

  # =========================================================================
  # -- Run matching ----------------------------------------------------------
  # =========================================================================

  matching_result <- reactiveVal(NULL)

  observeEvent(input$click_matching, {

    v       <- match_vect_data()
    treat   <- input$match_treatment
    covars  <- input$match_covars

    if (is.null(v) || !inherits(v, "SpatVector")) {
      showNotification(
        paste0("No valid spatial dataset loaded. Got: ",
               class(v)[1], ". Please load a vector file."),
        type = "error")
      return()
    }
    if (is.null(treat) || nchar(treat) == 0) {
      showNotification("Please select a treatment attribute.", type = "error")
      return()
    }
    if (is.null(covars) || length(covars) == 0) {
      showNotification("Please select at least one covariate.", type = "error")
      return()
    }

    # -- Collect matchit arguments from UI inputs ----------------------------
    mi_args <- list()

    mi_args$method   <- input$mi_method
    mi_args$estimand <- input$mi_estimand

    # distance group
    if (!input$mi_method %in% c("exact","cem","cardinality")) {
      if (!is.null(input$mi_distance) && length(input$mi_distance) > 0) {
        mi_args$distance <- input$mi_distance
      }
      lnk <- input$mi_link
      if (!is.null(lnk) && nchar(lnk) > 0) mi_args$link <- lnk
    }

    # replace + ratio
    rep_val <- input$mi_replace
    if (!is.null(rep_val) && length(rep_val) > 0 && nchar(rep_val) > 0) {
      mi_args$replace <- as.logical(rep_val)
    }
    rat_val <- input$mi_ratio
    if (!is.null(rat_val) && length(rat_val) > 0 && !is.na(rat_val)) {
      mi_args$ratio <- as.integer(rat_val)
    }

    # caliper: read directly from inputs using isolate() to avoid
    # creating reactive dependencies that could invalidate match_vect_data()
    {
      cal_vec <- numeric(0)
      std_vec <- logical(0)
      all_cal_vars <- c(".distance", covars)
      for (cv in all_cal_vars) {
        safe   <- gsub("[^a-zA-Z0-9_]", "_", cv)
        val    <- isolate(input[[paste0("mi_cal_val_", safe)]])
        std    <- isolate(input[[paste0("mi_cal_std_", safe)]])
        if (!is.null(val) && length(val) > 0 && !is.na(val)) {
          if (cv == ".distance") {
            cal_vec <- c(cal_vec, val)
          } else {
            cal_vec <- c(cal_vec, setNames(val, cv))
          }
          std_vec <- c(std_vec, isTRUE(as.logical(std)))
        }
      }
      if (length(cal_vec) > 0 && all(is.numeric(cal_vec))) {
        mi_args$caliper     <- cal_vec
        mi_args$std.caliper <- std_vec
      }
    }

    # m.order
    mord <- input$mi_m_order
    if (!is.null(mord) && nchar(mord) > 0) mi_args$m.order <- mord

    # discard + reestimate
    disc <- input$mi_discard
    if (!is.null(disc) && disc != "none") {
      mi_args$discard <- disc
      reest <- input$mi_reestimate
      if (!is.null(reest) && length(reest) > 0) mi_args$reestimate <- as.logical(reest)
    }

    # exact + antiexact
    ex_val <- input$mi_exact
    if (!is.null(ex_val) && length(ex_val) > 0) {
      mi_args$exact <- as.formula(
        paste0("~", paste(ex_val, collapse = " + ")))
    }
    anti_val <- input$mi_antiexact
    if (!is.null(anti_val) && length(anti_val) > 0) {
      mi_args$antiexact <- as.formula(
        paste0("~", paste(anti_val, collapse = " + ")))
    }

    # mahvars
    mah_val <- input$mi_mahvars
    if (!is.null(mah_val) && length(mah_val) > 0) {
      mi_args$mahvars <- as.formula(
        paste0("~", paste(mah_val, collapse = " + ")))
    }

    # s.weights
    sw_val <- input$mi_sweights
    if (!is.null(sw_val) && nchar(sw_val) > 0) mi_args$s.weights <- sw_val

    # method-specific extras
    if (input$mi_method == "subclass") {
      sc_val <- input$mi_subclass
      if (!is.null(sc_val) && !is.na(sc_val)) mi_args$subclass <- as.integer(sc_val)
    }
    if (input$mi_method == "cem") {
      k2k_val <- input$mi_k2k
      if (!is.null(k2k_val) && length(k2k_val) > 0) mi_args$k2k <- as.logical(k2k_val)
    }

    # distance.options: parse text field as R expression if provided
    dist_opts <- input$mi_distance_options
    if (!is.null(dist_opts) && nchar(trimws(dist_opts)) > 0) {
      parsed <- tryCatch(
        eval(parse(text = dist_opts)),
        error = function(e) {
          showNotification(
            paste0("distance.options could not be parsed: ", conditionMessage(e)),
            type = "warning")
          NULL
        }
      )
      if (!is.null(parsed)) mi_args$distance.options <- parsed
    }

    # -- Unique ID: use selected field or auto-generate ----------------------
    uid_field <- input$match_uid_field
    if (is.null(uid_field) || nchar(uid_field) == 0) {
      uid_field <- NULL   # run_matching will create .uid automatically
    }

    # -- Run -----------------------------------------------------------------
    progress <- shiny::Progress$new()
    progress$set(message = "Running matchit()...", value = 0.2)
    on.exit(progress$close())

    treat_value <- input$match_treat_value

    result <- tryCatch(
      do.call(run_matching,
              c(list(x             = v,
                     col_treatment = treat,
                     col_covars    = covars,
                     col_uid       = uid_field,
                     treat_value   = treat_value),
                mi_args)),
      error = function(e) {
        list(matchit_obj  = NULL,
             matched_data = NULL,
             formula      = NULL,
             errors       = paste0("Fatal error: ", conditionMessage(e)))
      }
    )

    progress$set(value = 1, message = "Done.")
    matching_result(result)

    if (length(result$errors) > 0) {
      showNotification(
        paste(result$errors, collapse = "
"),
        type     = "warning",
        duration = 10
      )
    }

    if (!is.null(result$matched_data)) {
      n_matched <- nrow(result$matched_data)
      showNotification(
        paste0("Matching complete: ", n_matched, " matched units."),
        type = "message"
      )
    }
  })

  # =========================================================================
  # -- Matching evaluation: Card 1 - dropped units diagnostics --------------
  # =========================================================================

  output$match_eval_dropped_card_ui <- renderUI({
    res <- matching_result()
    if (is.null(res) || is.null(res$n_dropped) || res$n_dropped == 0) {
      return(NULL)
    }

    dropped <- res$dropped    # data.frame with row_id and reason
    miss    <- res$miss_cols

    div(class = "card",
      div(class = "card-title",
        span(class = "icon", "\u26a0"),
        paste0("Incomplete units dropped before matching (", res$n_dropped, ")")
      ),
      p(style = "font-size:13px; color:#666; margin-bottom:8px;",
        paste0(res$n_dropped, " unit(s) were excluded from matching because ",
               "they had missing or non-finite values in the following ",
               "covariate(s): ",
               paste(miss, collapse = ", "), ".")),
      p(style = "font-size:13px; color:#666; margin-bottom:12px;",
        "These units are not included in any matching results or balance ",
        "statistics below. Consider imputing missing values or removing ",
        "these covariates before re-running matching."),
      if (!is.null(dropped) && nrow(dropped) > 0) {
        tagList(
          strong(style = "font-size:13px;", "Dropped units:"),
          div(style = "margin-top:8px; max-height:200px; overflow-y:auto;",
            tableOutput("match_eval_dropped_table")
          )
        )
      } else { NULL }
    )
  })

  output$match_eval_dropped_table <- renderTable({
    res <- matching_result()
    req(!is.null(res) && !is.null(res$dropped))
    res$dropped
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

  # =========================================================================
  # -- Matching evaluation: Card 2 - interactive leaflet map ----------------
  # =========================================================================

  # Track which feature ID was last clicked
  selected_id <- reactiveVal(NULL)

  # Helper: convert SpatVector to sf.
  # .feature_id uses the UID column value (same as .match_ids now uses),
  # so partner lookup from .match_ids is direct with no translation.
  matched_sf <- reactive({
    res       <- matching_result()
    req(!is.null(res) && !is.null(res$matched_data))
    v         <- res$matched_data
    sf_obj    <- to_leaflet_sf(v)
    # .feature_id must match the ID system used in .match_ids.
    # Priority: .uid (auto-generated or user UID stored internally) >
    #           user uid_field > sequential fallback.
    uid_field <- isolate(input$match_uid_field)
    if (".uid" %in% names(sf_obj)) {
      sf_obj$.feature_id <- as.character(sf_obj[[".uid"]])
    } else if (!is.null(uid_field) && nchar(uid_field) > 0 &&
               uid_field %in% names(sf_obj)) {
      sf_obj$.feature_id <- as.character(sf_obj[[uid_field]])
    } else {
      sf_obj$.feature_id <- as.character(seq_len(nrow(sf_obj)))
    }
    sf_obj
  })

  output$match_eval_map_card_ui <- renderUI({
    res <- matching_result()

    if (is.null(res)) {
      return(div(class = "card",
        div(class = "card-title",
          span(class = "icon", "\U0001f5fa"), "Matched units map"),
        p(class = "placeholder-msg",
          "Run matching to see results here.")
      ))
    }

    if (is.null(res$matched_data)) {
      return(div(class = "card",
        div(class = "card-title",
          span(class = "icon", "\U0001f5fa"), "Matched units map"),
        p(style = "color:#c0392b; font-size:13px;",
          paste(c("Matching failed:", res$errors), collapse = " "))
      ))
    }

    if (!inherits(res$matched_data, "SpatVector")) {
      return(div(class = "card",
        div(class = "card-title",
          span(class = "icon", "\U0001f5fa"), "Matched units map"),
        p(style = "color:#888; font-size:13px;",
          "Map not available: input was not a spatial vector dataset.")
      ))
    }

    v           <- res$matched_data
    treat_col   <- isolate(input$match_treatment)
    treat_val   <- isolate(input$match_treat_value)
    n_treat <- if (!is.null(treat_col) && treat_col %in% names(v)) {
      tv  <- as.character(treat_val)
      col <- as.character(as.data.frame(v)[[treat_col]])
      sum(col == tv, na.rm = TRUE)
    } else { NA }
    n_control <- if (!is.na(n_treat)) nrow(v) - n_treat else NA

    div(class = "card",
      div(class = "card-title",
        span(class = "icon", "\U0001f5fa"), "Matched units map"
      ),
      p(style = "font-size:12px; color:#888; margin-bottom:8px;",
        "Click a unit to highlight it and its match(es). Click again or ",
        "click empty space to deselect."),
      div(class = "info-row", style = "margin-bottom:12px;",
        span(class = "info-pill", paste0("Total: ", nrow(v))),
        if (!is.na(n_treat))   span(class = "info-pill", paste0("Treated: ",  n_treat))   else NULL,
        if (!is.na(n_control)) span(class = "info-pill", paste0("Controls: ", n_control)) else NULL
      ),
      leafletOutput("match_eval_leaflet", height = "520px")
    )
  })

  # Colour helpers
  COL_TREAT   <- "#00AF9E"
  COL_CONTROL <- "#E67E22"
  ALPHA_DIM   <- 0.15   # opacity for non-selected units when something is selected
  ALPHA_FULL  <- 0.7    # normal fill opacity

  feature_colours <- reactive({
    sf_obj <- matched_sf()
    if (is.null(sf_obj) || !inherits(sf_obj, "sf")) return(character(0))
    treat_col <- isolate(input$match_treatment)
    treat_val <- isolate(input$match_treat_value)
    df        <- sf::st_drop_geometry(sf_obj)
    if (!is.null(treat_col) && treat_col %in% names(df) &&
        !is.null(treat_val)) {
      tv <- as.character(treat_val)
      ifelse(as.character(df[[treat_col]]) == tv, COL_TREAT, COL_CONTROL)
    } else {
      rep("#3498db", nrow(df))
    }
  })

  # Initial map render
  output$match_eval_leaflet <- renderLeaflet({
    sf_obj <- matched_sf()
    req(!is.null(sf_obj) && inherits(sf_obj, "sf"))
    cols   <- feature_colours()
    if (length(cols) == 0) { return(leaflet::leaflet()) }
    geom_t <- unique(sf::st_geometry_type(sf_obj))
    is_pt  <- any(geom_t %in% c("POINT", "MULTIPOINT"))

    m <- leaflet::leaflet(sf_obj) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                                options = leaflet::tileOptions(opacity = 0.6))

    if (is_pt) {
      m <- m |>
        leaflet::addCircleMarkers(
          layerId    = ~.feature_id,
          color      = cols,
          fillColor  = cols,
          fillOpacity = ALPHA_FULL,
          opacity    = 1,
          radius     = 6,
          weight     = 1.5,
          stroke     = TRUE
        )
    } else {
      m <- m |>
        leaflet::addPolygons(
          layerId     = ~.feature_id,
          fillColor   = cols,
          fillOpacity = ALPHA_FULL,
          color       = "white",
          weight      = 1,
          smoothFactor = 0.5
        )
    }
    m
  })

  # Flag to suppress the background click that fires right after a shape click
  shape_just_clicked <- reactiveVal(FALSE)

  observeEvent(input$match_eval_leaflet_shape_click, {
    click   <- input$match_eval_leaflet_shape_click
    clicked <- click$id
    current <- selected_id()

    # Signal that a shape was just clicked so the map click observer ignores
    # the event that fires immediately after
    shape_just_clicked(TRUE)

    # Toggle: clicking same unit again deselects
    if (!is.null(current) && clicked %in% current) {
      selected_id(NULL)
    } else {
      sf_obj        <- matched_sf()
      df            <- sf::st_drop_geometry(sf_obj)
      match_ids_str <- df$.match_ids[df$.feature_id == clicked]
      partners      <- unlist(strsplit(match_ids_str, ","))
      partners      <- trimws(partners[nchar(trimws(partners)) > 0])
      # .match_ids now contains UID values, same as .feature_id,
      # so no translation needed
      selected_id(c(clicked, partners))
    }
  }, ignoreInit = TRUE)

  # Deselect on map background click - but only if no shape was just clicked
  observeEvent(input$match_eval_leaflet_click, {
    if (isTRUE(shape_just_clicked())) {
      shape_just_clicked(FALSE)   # consume the flag, do nothing
    } else {
      selected_id(NULL)
    }
  }, ignoreInit = TRUE)

  # Update feature styles when selection changes.
  # Use leafletProxy to avoid full re-render (which would reset layerIds).
  observe({
    sf_obj <- matched_sf()
    if (is.null(sf_obj) || !inherits(sf_obj, "sf")) return()
    cols   <- feature_colours()
    sel    <- selected_id()
    df     <- sf::st_drop_geometry(sf_obj)
    geom_t <- unique(sf::st_geometry_type(sf_obj))
    is_pt  <- any(geom_t %in% c("POINT", "MULTIPOINT"))

    has_sel  <- !is.null(sel) && length(sel) > 0
    is_sel   <- if (has_sel) df$.feature_id %in% sel else rep(FALSE, nrow(df))

    fill_op  <- ifelse(has_sel, ifelse(is_sel, ALPHA_FULL, ALPHA_DIM), ALPHA_FULL)
    stroke_c <- ifelse(is_sel, "black", "white")
    stroke_w <- ifelse(is_sel, 3, 1)
    radius   <- ifelse(is_sel, 9, 6)
    line_op  <- pmin(fill_op + 0.2, 1)

    proxy <- leaflet::leafletProxy("match_eval_leaflet", data = sf_obj)

    if (is_pt) {
      proxy |>
        leaflet::clearMarkers() |>
        leaflet::addCircleMarkers(
          layerId     = ~.feature_id,
          color       = stroke_c,
          fillColor   = cols,
          fillOpacity = fill_op,
          opacity     = line_op,
          radius      = radius,
          weight      = stroke_w,
          stroke      = TRUE
        )
    } else {
      proxy |>
        leaflet::clearShapes() |>
        leaflet::addPolygons(
          layerId      = ~.feature_id,
          fillColor    = cols,
          fillOpacity  = fill_op,
          color        = stroke_c,
          weight       = stroke_w,
          smoothFactor = 0.5,
          options      = leaflet::pathOptions(clickable = TRUE)
        )
    }
  })

  # =========================================================================
  # -- Matching evaluation: Card 3 - attribute table for selected units -----
  # =========================================================================

  output$match_eval_attr_card_ui <- renderUI({
    sel <- selected_id()
    if (is.null(sel) || length(sel) == 0) return(NULL)

    div(class = "card",
      div(class = "card-title",
        span(class = "icon", "\U0001f4cb"),
        paste0("Selected units (", length(sel), ")")
      ),
      p(style = "font-size:12px; color:#888; margin-bottom:8px;",
        "Clicked unit and its matched partner(s)."),
      div(style = "overflow-x:auto;",
        tableOutput("match_eval_attr_table")
      )
    )
  })

  output$match_eval_attr_table <- renderTable({
    sel    <- selected_id()
    req(!is.null(sel) && length(sel) > 0)
    sf_obj <- matched_sf()
    df     <- sf::st_drop_geometry(sf_obj)
    treat_col <- isolate(input$match_treatment)

    # Subset to selected rows and drop internal columns
    sub <- df[df$.feature_id %in% sel, , drop = FALSE]
    # Show .uid and .match_ids (useful for tracing pairs);
    # hide .subclass (internal matchit grouping), .weights, .distance,
    # .feature_id, .wkt (internal app columns).
    drop_cols <- c(".feature_id", ".subclass", ".weights", ".distance", ".wkt")
    sub <- sub[, setdiff(names(sub), drop_cols), drop = FALSE]

    # Add a role column using treat_value for comparison (supports character/factor)
    treat_val_sel <- isolate(input$match_treat_value)
    if (!is.null(treat_col) && treat_col %in% names(sub) &&
        !is.null(treat_val_sel)) {
      tv <- as.character(treat_val_sel)
      sub$.role <- ifelse(as.character(sub[[treat_col]]) == tv,
                          "Treated", "Control")
      sub <- sub[, c(".role", setdiff(names(sub), ".role")), drop = FALSE]
    }
    sub
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # =========================================================================
  # -- Matching evaluation: Card 4 - cobalt diagnostics ---------------------
  # =========================================================================

  # Reactive: compute all cobalt diagnostics once after matching
  match_diagnostics <- reactive({
    res <- matching_result()
    req(!is.null(res) && !is.null(res$matchit_obj))
    m_obj <- res$matchit_obj

    if (!requireNamespace("cobalt", quietly = TRUE)) {
      return(list(error = "Package 'cobalt' is not installed."))
    }

    method <- m_obj$info$method

    # Balance table (always available)
    bal <- tryCatch(
      get_balance_table(m_obj),
      error = function(e) { list(error = conditionMessage(e)) }
    )

    # Love plot (always available except for exact matching)
    lp <- if (!is.null(method) && method == "exact") {
      NULL
    } else {
      tryCatch(
        get_love_plot(m_obj),
        error = function(e) { NULL }
      )
    }

    # Overlap plots: one per covariate, skip for exact/cem
    no_overlap <- c("exact", "cem")
    overlap_plots <- if (!is.null(method) && method %in% no_overlap) {
      list()
    } else {
      tryCatch(
        get_balance_plots(m_obj),
        error = function(e) { list() }
      )
    }

    list(
      balance       = bal,
      love_plot     = lp,
      overlap_plots = overlap_plots,
      method        = method
    )
  })

  # Card UI
  output$match_eval_diag_card_ui <- renderUI({
    res <- matching_result()
    if (is.null(res) || is.null(res$matchit_obj)) return(NULL)

    diag <- match_diagnostics()

    if (!is.null(diag$error)) {
      return(div(class = "card",
        div(class = "card-title",
          span(class = "icon", "\U0001f4ca"), "Matching diagnostics"),
        p(style = "color:#c0392b; font-size:13px;", paste0("! ", diag$error))
      ))
    }

    n_overlap <- length(diag$overlap_plots)

    div(class = "card",
      div(class = "card-title",
        span(class = "icon", "\U0001f4ca"),
        paste0("Matching diagnostics (method: ", diag$method, ")")
      ),

      # -- Balance table ----
      strong(style = "font-size:13px;", "Covariate balance"),
      p(style = "font-size:12px; color:#888; margin:4px 0 8px 0;",
        "Std. mean difference < 0.1 indicates good balance (|SMD| threshold shown)."),
      div(style = "overflow-x:auto; margin-bottom:12px;",
        tableOutput("match_diag_bal_table")
      ),

      # Sample sizes
      if (!is.null(diag$balance$sample_sizes)) {
        tagList(
          strong(style = "font-size:13px;", "Sample sizes"),
          div(style = "overflow-x:auto; margin-bottom:16px;",
            tableOutput("match_diag_ss_table")
          )
        )
      } else { NULL },

      div(class = "section-divider"),

      # -- Love plot ----
      if (!is.null(diag$love_plot)) {
        tagList(
          strong(style = "font-size:13px;", "Love plot"),
          p(style = "font-size:12px; color:#888; margin:4px 0 8px 0;",
            "Standardised mean differences before and after matching. ",
            "Dashed line at |SMD| = 0.1."),
          plotOutput("match_diag_love_plot",
            height = paste0(max(200, 30 * length(diag$balance$balance_df[[1]])), "px")),
          div(class = "section-divider")
        )
      } else { NULL },

      # -- Overlap plots ----
      if (n_overlap > 0) {
        tagList(
          strong(style = "font-size:13px;",
            paste0("Covariate overlap (", n_overlap, " variable",
                   if (n_overlap > 1) "s" else "", ")")),
          p(style = "font-size:12px; color:#888; margin:4px 0 8px 0;",
            "Distribution before (Unadjusted) and after (Adjusted) matching, ",
            "by treatment group."),
          uiOutput("match_diag_overlap_plots_ui")
        )
      } else { NULL }
    )
  })

  # Balance table render
  output$match_diag_bal_table <- renderTable({
    diag <- match_diagnostics()
    req(!is.null(diag$balance) && is.null(diag$balance$error))
    diag$balance$balance_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Sample sizes render
  output$match_diag_ss_table <- renderTable({
    diag <- match_diagnostics()
    req(!is.null(diag$balance$sample_sizes))
    ss <- diag$balance$sample_sizes
    cbind(Group = rownames(ss), ss)
  }, striped = TRUE, bordered = TRUE)

  # Love plot render
  output$match_diag_love_plot <- renderPlot({
    diag <- match_diagnostics()
    req(!is.null(diag$love_plot))
    print(diag$love_plot)
  }, res = 96, bg = "white")

  # Overlap plots: one plotOutput per covariate, rendered dynamically
  output$match_diag_overlap_plots_ui <- renderUI({
    diag <- match_diagnostics()
    nms  <- names(diag$overlap_plots)
    if (length(nms) == 0) return(NULL)

    plot_outputs <- lapply(nms, function(nm) {
      pid <- paste0("match_diag_bal_", make.names(nm))
      div(style = "margin-bottom:16px;",
        strong(style = "font-size:12px; color:#555;", nm),
        plotOutput(pid, height = "220px")
      )
    })
    do.call(tagList, plot_outputs)
  })

  # Register one renderPlot per overlap plot
  observe({
    diag <- match_diagnostics()
    nms  <- names(diag$overlap_plots)
    lapply(nms, function(nm) {
      local({
        nm_local <- nm
        pid      <- paste0("match_diag_bal_", make.names(nm_local))
        output[[pid]] <- renderPlot({
          p <- diag$overlap_plots[[nm_local]]
          req(!is.null(p))
          print(p)
        }, res = 96, bg = "white")
      })
    })
  })

  # =========================================================================
  # -- Matching evaluation: Card 5 - save matched units to file -------------
  # =========================================================================

  output$match_eval_save_card_ui <- renderUI({
    res <- matching_result()
    if (is.null(res) || is.null(res$matched_data)) return(NULL)

    div(class = "card",
      div(class = "card-title",
        span(class = "icon", "\U0001f4be"),
        "Save matched units"
      ),
      p(style = "font-size:13px; color:#666; margin-bottom:16px;",
        "Save the matched SpatVector to a GeoPackage file (.gpkg) for use ",
        "in later analysis or to skip matching when re-opening the app."),
      div(style = "display:flex; align-items:flex-end; gap:10px;",
        div(style = "flex:1;",
          textInput("match_save_path",
            label       = "Output file path (without extension)",
            placeholder = "e.g. C:/Data/matched_units",
            width       = "100%")
        ),
        div(style = "margin-bottom:15px;",
          actionButton("match_save_btn", "Save .gpkg",
            class = "btn btn-success btn-sm")
        )
      ),
      uiOutput("match_save_status_ui")
    )
  })

  observeEvent(input$match_save_btn, {
    res  <- matching_result()
    path <- trimws(input$match_save_path)

    if (is.null(res) || is.null(res$matched_data)) {
      output$match_save_status_ui <- renderUI(
        p(style = "color:#c0392b; font-size:12px; margin-top:4px;",
          "! No matched data available.")
      )
      return()
    }

    if (nchar(path) == 0) {
      output$match_save_status_ui <- renderUI(
        p(style = "color:#c0392b; font-size:12px; margin-top:4px;",
          "! Please enter a file path.")
      )
      return()
    }

    out_path <- paste0(path, ".gpkg")

    tryCatch({
      terra::writeVector(res$matched_data, out_path, overwrite = TRUE)
      output$match_save_status_ui <- renderUI(
        p(style = "color:#1e8449; font-size:12px; margin-top:4px;",
          paste0("v Saved: ", out_path))
      )
    }, error = function(e) {
      output$match_save_status_ui <- renderUI(
        p(style = "color:#c0392b; font-size:12px; margin-top:4px;",
          paste0("! Could not save file: ", conditionMessage(e)))
      )
    })
  }, ignoreInit = TRUE)

  # =========================================================================