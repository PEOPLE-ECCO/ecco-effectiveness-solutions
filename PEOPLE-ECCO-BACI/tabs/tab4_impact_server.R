# =============================================================================
# tab4_impact_server.R
# Server logic for Tab 4: Impact evaluation
# Sourced inside server() with local=TRUE - has access to input/output/session

  # -- Tab 6: Impact evaluation: matched pairs input ------------------------
  # =========================================================================

  # Reactive: geometry type of the BACI input vector
  # Used in the impact evaluation source slots to adapt raster extraction
  # controls (point -> resampling method; polygon/line -> function).
  # This is independent of geom_info() which refers to the Tab 2 vector.
  baci_geom_type <- reactive({
    v <- baci_vect_data()
    if (is.null(v)) return("Points")   # safe default
    raw <- tryCatch(unique(terra::geomtype(v)), error = function(e) { "points" })
    geom_lookup <- c(
      polygons = "Polygon", polygon = "Polygon",
      lines    = "Lines",   line    = "Lines",
      points   = "Points",  point   = "Points"
    )
    canon <- vapply(raw, function(t) {
      lbl <- geom_lookup[t]
      if (is.na(lbl)) { t } else { unname(lbl) }
    }, character(1))
    unname(canon[1])   # return first type (mixed not expected)
  })

  # Reactive: matched pairs vector from either matching output or file
  baci_vect_data <- reactive({
    if (input$baci_input_source == "from_matching") {
      res <- matching_result()
      if (is.null(res) || is.null(res$matched_data)) return(NULL)
      res$matched_data
    } else {
      if (is.null(input$baci_vect_file)) return(NULL)
      baci_vect_from_file()
    }
  })

  baci_vect_from_file <- reactive({
    req(input$baci_vect_file)
    paths    <- input$baci_vect_file$datapath
    names_up <- input$baci_vect_file$name
    new_paths <- file.path(dirname(paths), names_up)
    file.rename(paths, new_paths)
    is_shp <- endsWith(tolower(names_up), ".shp")
    entry  <- if (any(is_shp)) { new_paths[is_shp] } else { new_paths[1] }
    tryCatch(terra::vect(entry), error = function(e) { NULL })
  })

  # Status / metadata
  output$baci_vect_status_ui <- renderUI({
    if (input$baci_input_source == "from_matching") {
      res <- matching_result()
      if (is.null(res) || is.null(res$matched_data)) {
        return(p(class = "placeholder-msg",
                 "No matching output yet. Run matching in the Matching analysis
                  tab first."))
      }
      v <- res$matched_data
      div(class = "info-row", style = "margin-top:8px;",
        span(class = "info-pill", paste0(nrow(v), " matched units")),
        span(class = "info-pill", paste0(ncol(as.data.frame(v)), " attributes")),
        span(class = "info-pill", "From matching output")
      )
    } else {
      if (is.null(input$baci_vect_file)) return(NULL)
      v <- baci_vect_from_file()
      if (is.null(v)) {
        return(p(style = "color:#c0392b; font-size:12px; margin-top:8px;",
                 "! Could not read file."))
      }
      crs_name <- terra::crs(v, describe = TRUE)$name
      div(class = "info-row", style = "margin-top:8px;",
        span(class = "info-pill", paste0(nrow(v), " features")),
        span(class = "info-pill", paste0(ncol(as.data.frame(v)), " attributes")),
        if (!is.na(crs_name) && nchar(crs_name) > 0) {
          span(class = "info-pill", crs_name)
        } else { NULL }
      )
    }
  })

  # BACI plot card: attr selector at top, plot below
  output$baci_plot_card_ui <- renderUI({
    v <- baci_vect_data()
    if (is.null(v)) return(NULL)
    attr_names <- names(v)
    div(class = "card", style = "height:100%;",
      div(class = "card-title",
        span(class = "icon", "\U0001f5fa"),
        "Vector preview"
      ),
      if (length(attr_names) > 0) {
        selectInput("baci_plot_attr",
          label    = "Attribute to visualise",
          choices  = setNames(attr_names, attr_names),
          selected = attr_names[1],
          width    = "100%")
      } else { NULL },
      plotOutput("baci_vect_plot", height = "480px")
    )
  })

  output$baci_vect_plot <- renderPlot({
    v <- baci_vect_data()
    req(!is.null(v))
    attr <- input$baci_plot_attr
    if (!is.null(attr) && attr %in% names(v)) {
      terra::plot(v, attr, main = attr)
    } else {
      terra::plot(v)
    }
  }, res = 96, bg = "white")

  # -- Column role selectors for BACI input vector -------------------------
  output$baci_col_selectors_ui <- renderUI({
    v <- baci_vect_data()
    if (is.null(v)) return(NULL)
    attr_names <- names(v)
    pre_treat <- if (!is.null(input$match_treatment) &&
                     input$match_treatment %in% attr_names) {
      input$match_treatment
    } else { attr_names[1] }
    pre_uid <- if (!is.null(input$match_uid_field) &&
                   nchar(input$match_uid_field) > 0 &&
                   input$match_uid_field %in% attr_names) {
      input$match_uid_field
    } else { "" }
    pre_match_ids <- if (".match_ids" %in% attr_names) { ".match_ids" } else { "" }
    tagList(
      selectInput("baci_col_treatment",
        label    = "Treatment attribute",
        choices  = setNames(attr_names, attr_names),
        selected = pre_treat, width = "100%"),
      uiOutput("baci_treat_value_ui"),
      selectInput("baci_col_uid",
        label    = "Unique feature ID",
        choices  = c("None" = "", setNames(attr_names, attr_names)),
        selected = pre_uid, width = "100%"),
      selectInput("baci_col_match_ids",
        label    = "Matched partner ID column",
        choices  = c("None" = "", setNames(attr_names, attr_names)),
        selected = pre_match_ids, width = "100%")
    )
  })

  output$baci_treat_value_ui <- renderUI({
    v     <- baci_vect_data()
    treat <- input$baci_col_treatment
    if (is.null(v) || is.null(treat) || !treat %in% names(v)) return(NULL)
    vals        <- sort(unique(na.omit(as.data.frame(v)[[treat]])))
    if (length(vals) < 2) return(NULL)
    default_val <- if (all(vals %in% c(0, 1))) { "1" } else { as.character(vals[length(vals)]) }
    selectInput("baci_treat_value",
      label    = "Value indicating treatment (impact)",
      choices  = setNames(as.character(vals), as.character(vals)),
      selected = default_val, width = "100%")
  })

  # =========================================================================
  # -- Tab 6: Impact evaluation method selection ----------------------------
  # =========================================================================

  output$baci_method_card_ui <- renderUI({
    v <- baci_vect_data()
    if (is.null(v)) return(NULL)

    div(class = "card",
      div(class = "card-title",
        span(class = "icon", "\U0001f9ea"),
        "Impact assessment method"
      ),
      p(style = "font-size:13px; color:#666; margin-bottom:16px;",
        "Select the method for impact assessment. Additional inputs will
         appear based on the selected method."),

      selectInput("baci_method",
        label    = "Method",
        choices  = c(
          "-- Select a method --"       = "",
          "BACI contrast and p-value"   = "baci_contrast"
        ),
        selected = "",
        width    = "480px"
      ),

      # Method-specific inputs appear here
      uiOutput("baci_method_inputs_ui")
    )
  })

  output$baci_method_inputs_ui <- renderUI({
    method <- input$baci_method
    if (is.null(method) || nchar(method) == 0) return(NULL)

    if (method == "baci_contrast") {
      div(style = "margin-top:8px;",
        radioButtons("baci_design",
          label   = "Study design",
          choices = c(
            "Before-After-Control-Impact assessment" = "baci",
            "Control-Impact assessment"              = "ci"
          ),
          selected = "baci"
        )
      )
    } else {
      NULL
    }
  })

  # =========================================================================
  # -- Tab 6: Impact source slot helpers ------------------------------------
  # =========================================================================

  # Generic UI builder for one impact source slot (before / after / effect).
  # slot_id : short string used as input ID prefix, e.g. "before", "after"
  # slot_label: display label, e.g. 'Source type "before"'
  make_source_slot_ui <- function(slot_id, slot_label) {
    src_type_id <- paste0("ie_srctype_", slot_id)
    src_val     <- input[[src_type_id]]
    src_sel     <- if (!is.null(src_val) && nchar(src_val) > 0) src_val else ""

    # Source type dropdown
    type_row <- selectInput(src_type_id,
      label   = slot_label,
      choices = c(
        "-- Select --"           = "",
        "From vector of matches" = "from_vector",
        "Local raster"           = "Local raster",
        "URL raster"             = "URL raster",
        "Local vector"           = "Local vector",
        "openEO collection"      = "openEO"
      ),
      selected = src_sel,
      width    = "100%"
    )

    # Source-specific controls
    extra <- if (is.null(src_val) || nchar(src_val) == 0) {
      NULL

    } else if (src_val == "from_vector") {
      # Multi-select attribute selector from the matched pairs vector
      v            <- baci_vect_data()
      attr_choices <- if (!is.null(v)) names(v) else character(0)
      attr_id      <- paste0("ie_attr_", slot_id)
      cur_sel      <- input[[attr_id]]
      valid_sel    <- cur_sel[cur_sel %in% attr_choices]
      if (length(attr_choices) > 0) {
        tagList(
          selectInput(attr_id,
            label    = paste0("Attribute(s) in matched pairs vector (",
                              slot_id, ")"),
            choices  = setNames(attr_choices, attr_choices),
            selected = if (length(valid_sel) > 0) valid_sel else NULL,
            multiple = TRUE,
            width    = "100%"),
          # Name-match warning for before/after slots (not effect)
          if (slot_id %in% c("before", "after")) {
            uiOutput(paste0("ie_namematch_warn_", slot_id))
          } else { NULL }
        )
      } else {
        p(style = "color:#e67e22; font-size:12px;",
          "Load matched pairs vector in Card 1 first.")
      }

    } else {
      # Raster or vector file path - reuse matching covariates pattern
      is_raster <- src_val %in% c("Local raster", "URL raster")
      is_vec    <- src_val == "Local vector"
      path_id   <- paste0("ie_path_", slot_id)
      add_id    <- paste0("ie_add_",  slot_id)

      path_lbl <- if (src_val == "URL raster") {
        "URL to raster file"
      } else if (is_vec) {
        "Path to vector file (.shp, .geojson, .gpkg)"
      } else {
        "Path to raster file (.tif)"
      }
      path_ph <- if (src_val == "URL raster") {
        "e.g. https://example.com/data/layer.tif"
      } else if (is_vec) {
        "e.g. C:/Data/layer.shp"
      } else {
        "e.g. C:/Data/layer.tif"
      }

      # Status / confirmed source info
      conf <- ie_source_list()[[slot_id]]
      if (!is.null(conf)) {
        # Show confirmed source with metadata and controls
        make_confirmed_source_ui(slot_id, conf)
      } else {
        div(style = "margin-top:8px;",
          div(style = "display:flex; align-items:flex-end; gap:10px;",
            div(style = "flex:1;",
              textInput(path_id, label = path_lbl,
                placeholder = path_ph, width = "100%")
            ),
            div(style = "margin-bottom:15px;",
              actionButton(add_id, "Confirm",
                class = "btn btn-primary btn-sm")
            )
          ),
          uiOutput(paste0("ie_status_", slot_id))
        )
      }
    }

    tagList(type_row, extra)
  }

  # Renders confirmed source details + raster controls (resampling / layers)
  # Mirrors the confirmed_rows rendering in matchlyr_rows_ui
  make_confirmed_source_ui <- function(slot_id, r) {
    reset_id <- paste0("ie_reset_", slot_id)

    meta <- if (r$source_type %in% c("Local raster", "URL raster",
                                      "PEOPLE-ECCO dataset")) {
      band_lbl <- if (r$nlyr == 1) "1 band" else paste0(r$nlyr, " bands")
      dim_lbl  <- paste0(r$nrow, " x ", r$ncol, " px")
      res_lbl  <- paste0("res: ", r$res[1], " x ", r$res[2])
      crs_pill <- if (!is.na(r$crs) && nchar(r$crs) > 0) {
        span(class = "info-pill", paste0("CRS: ", r$crs))
      } else { NULL }
      div(class = "info-row", style = "margin-top:4px;",
        span(class = "info-pill", r$source_type),
        span(class = "info-pill", band_lbl),
        span(class = "info-pill", dim_lbl),
        span(class = "info-pill", res_lbl),
        crs_pill
      )
    } else {
      crs_pill <- if (!is.na(r$crs) && nchar(r$crs) > 0) {
        span(class = "info-pill", paste0("CRS: ", r$crs))
      } else { NULL }
      div(class = "info-row", style = "margin-top:4px;",
        span(class = "info-pill", r$source_type),
        span(class = "info-pill", paste0(r$nfeat, " features")),
        crs_pill
      )
    }

    # Per-slot controls (resampling / layers for raster; function for vector)
    controls <- if (r$source_type %in% c("Local raster", "URL raster",
                                          "PEOPLE-ECCO dataset")) {
      geom     <- tryCatch(baci_geom_type(), error = function(e) { "Points" })
      is_point <- length(geom) == 1 && geom == "Points"
      fun_val  <- input[[paste0("ie_fun_", slot_id)]]

      tagList(
        div(style = "margin-top:12px; display:flex; gap:24px; align-items:flex-start;",
          div(style = "min-width:200px;",
            if (is_point) {
              selectInput(paste0("ie_resamp_", slot_id),
                label    = "Resampling method",
                choices  = c("bilinear", "nearest"),
                selected = r$resamp, width = "100%")
            } else {
              selectInput(paste0("ie_fun_", slot_id),
                label    = "Function",
                choices  = c("mean","median","sd","min","max",
                             "sum","simple","bilinear","fraction","custom"),
                selected = r$fun, width = "100%")
            }
          ),
          div(style = "flex:1;",
            selectInput(paste0("ie_sellyr_", slot_id),
              label    = "Layer(s) to use",
              choices  = r$lyr_choices,
              selected = r$sel_lyr,
              multiple = TRUE, width = "100%")
          )
        ),
        uiOutput(paste0("ie_row2_", slot_id)),
        # Chunked extraction option
        div(style = "display:flex; align-items:center; gap:16px; margin-top:8px;",
          checkboxInput(paste0("ie_use_chunks_", slot_id),
            label = "Chunked extraction",
            value = isTRUE(r$use_chunks)),
          conditionalPanel(
            condition = paste0("input['ie_use_chunks_", slot_id, "'] == true"),
            div(style = "min-width:160px;",
              numericInput(paste0("ie_chunk_size_", slot_id),
                label = "Chunk size (features)",
                value = if (!is.null(r$chunk_size)) r$chunk_size else 500L,
                min   = 1, step = 100, width = "100%")
            )
          )
        )
      )
    } else {
      # Local vector: function selector
      {
        # count: only meaningful when units=polygons, covariate=points
        cov_geom   <- r$geom_type
        baci_v     <- baci_vect_data()
        units_geom <- if (!is.null(baci_v)) {
          as.character(unique(terra::geomtype(baci_v)))
        } else { "" }
        show_count <- any(grepl("polygon", tolower(units_geom))) &&
                      grepl("point", tolower(cov_geom), ignore.case = TRUE)
        fun_choices <- c("mean","median","sd","min","max","sum","minDistance")
        if (show_count) { fun_choices <- c(fun_choices, "count") }
        selectInput(paste0("ie_vecfun_", slot_id),
          label    = "Function",
          choices  = fun_choices,
          selected = if (!is.null(r$vec_fun) && r$vec_fun %in% fun_choices) {
            r$vec_fun
          } else { "mean" },
          width = "100%")
      }
    }

    div(
      # Path display + reset button
      div(style = "display:flex; align-items:center; gap:10px; margin-top:8px;",
        div(style = paste0(
          "flex:1; background:#f7f9fb; border:1px solid #e0e4ea;",
          "border-radius:6px; padding:7px 12px; font-size:13px; color:#555;",
          "white-space:nowrap; overflow:hidden; text-overflow:ellipsis;"), r$path),
        span(style = "color:#1e8449; font-size:16px; font-weight:700;", "v"),
        actionButton(reset_id, label = "x", class = "btn btn-sm",
          style = paste0("padding:2px 8px; font-size:12px; background:#fdedec;",
                         "color:#c0392b; border:1px solid #f5b7b1; border-radius:4px;"))
      ),
      meta,
      controls
    )
  }

  # Reactive list holding confirmed external sources keyed by slot_id
  ie_source_list <- reactiveVal(list())

  # Register observers for each named slot
  ie_slots <- c("before", "after", "effect")

  lapply(ie_slots, function(slot_id) {
    local({
      sid <- slot_id

      # Confirm button
      observeEvent(input[[paste0("ie_add_", sid)]], {
        src  <- input[[paste0("ie_srctype_", sid)]]
        path <- trimws(input[[paste0("ie_path_",    sid)]])
        result <- if (!is.null(src) && src == "Local raster") {
          validate_local_raster(path)
        } else if (!is.null(src) && src == "URL raster") {
          validate_url_raster(path)
        } else if (!is.null(src) && src == "Local vector") {
          validate_local_vector(path)
        } else {
          list(ok = FALSE, msg = "Please select a source type.")
        }
        if (result$ok) {
          result$use_chunks <- FALSE
          result$chunk_size <- 500L
          current <- ie_source_list()
          current[[sid]] <- result
          ie_source_list(current)
          output[[paste0("ie_status_", sid)]] <- renderUI(NULL)
        } else if (nchar(result$msg) > 0) {
          local({
            m <- result$msg
            output[[paste0("ie_status_", sid)]] <- renderUI(
              p(style = "color:#c0392b; font-size:12px; margin:4px 0 0 0;",
                paste0("! ", m))
            )
          })
        }
      }, ignoreInit = TRUE)

      # Reset button
      observeEvent(input[[paste0("ie_reset_", sid)]], {
        current <- ie_source_list()
        current[[sid]] <- NULL
        ie_source_list(current)
      }, ignoreInit = TRUE)

      # Sync raster controls
      observeEvent(input[[paste0("ie_resamp_", sid)]], {
        current <- ie_source_list()
        if (!is.null(current[[sid]])) {
          current[[sid]]$resamp <- input[[paste0("ie_resamp_", sid)]]
          ie_source_list(current)
        }
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("ie_fun_", sid)]], {
        current <- ie_source_list()
        if (!is.null(current[[sid]])) {
          current[[sid]]$fun <- input[[paste0("ie_fun_", sid)]]
          ie_source_list(current)
        }
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("ie_sellyr_", sid)]], {
        current <- ie_source_list()
        if (!is.null(current[[sid]])) {
          current[[sid]]$sel_lyr <- input[[paste0("ie_sellyr_", sid)]]
          ie_source_list(current)
        }
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("ie_vecfun_", sid)]], {
        current <- ie_source_list()
        if (!is.null(current[[sid]])) {
          current[[sid]]$vec_fun <- input[[paste0("ie_vecfun_", sid)]]
          ie_source_list(current)
        }
      }, ignoreInit = TRUE)

      # Row 2 (fraction/custom/na.rm) - separate output to avoid reset bug
      output[[paste0("ie_row2_", sid)]] <- renderUI({
        conf <- ie_source_list()[[sid]]
        if (is.null(conf)) return(NULL)
        if (!conf$source_type %in% c("Local raster","URL raster","PEOPLE-ECCO dataset")) {
          return(NULL)
        }
        geom     <- tryCatch(baci_geom_type(), error = function(e) { "Points" })
        is_point <- length(geom) == 1 && geom == "Points"
        if (is_point) return(NULL)
        fun_val  <- input[[paste0("ie_fun_", sid)]]
        cur_fun  <- if (!is.null(fun_val)) fun_val else conf$fun
        stored   <- isolate(ie_source_list())
        na_rm    <- if (!is.null(stored[[sid]]$na_rm)) { stored[[sid]]$na_rm } else { "TRUE" }
        if (cur_fun == "custom") {
          textInput(paste0("ie_custom_", sid),
            label = "Custom function",
            placeholder = "e.g. function(x) mean(x, na.rm=TRUE)",
            width = "100%")
        } else if (cur_fun == "fraction") {
          textInput(paste0("ie_fraction_", sid),
            label = "Fraction condition",
            placeholder = "e.g. x > 30",
            width = "360px")
        } else {
          selectInput(paste0("ie_narm_", sid),
            label   = "na.rm",
            choices = c("TRUE","FALSE"),
            selected = na_rm, width = "180px")
        }
      })
    })
  })

  # Name-match warnings for before/after multi-select
  # Renders reactively as the user changes selections in either slot
  output$ie_namematch_warn_before <- renderUI({
    baci_namematch_warning()
  })
  output$ie_namematch_warn_after <- renderUI({
    baci_namematch_warning()
  })

  baci_namematch_warning <- reactive({
    before_sel <- input[["ie_attr_before"]]
    after_sel  <- input[["ie_attr_after"]]
    if (is.null(before_sel) || is.null(after_sel)) return(NULL)
    if (length(before_sel) <= 1 && length(after_sel) <= 1) return(NULL)
    if (length(before_sel) != length(after_sel)) {
      return(p(style = "color:#c0392b; font-size:12px; margin:4px 0 0 0;",
        paste0("! Number of selected attributes differs: ",
               length(before_sel), " before vs ",
               length(after_sel), " after.")))
    }
    # Check name match: strip _before/_after suffixes and compare
    strip <- function(x, sfx) sub(paste0("_", sfx, "$"), "", x)
    b_base <- strip(before_sel, "before")
    a_base <- strip(after_sel,  "after")
    if (!identical(sort(b_base), sort(a_base))) {
      return(p(style = "color:#e67e22; font-size:12px; margin:4px 0 0 0;",
        "! Attribute names do not match after stripping _before/_after suffixes. ",
        "Variables will be paired by selection order."))
    }
    NULL
  })

  # Render the method inputs UI (extended to include source slots)
  output$baci_method_inputs_ui <- renderUI({
    method <- input$baci_method
    if (is.null(method) || nchar(method) == 0) return(NULL)

    if (method == "baci_contrast") {
      design <- input$baci_design

      tagList(
        div(style = "margin-top:8px;",
          radioButtons("baci_design",
            label   = "Study design",
            choices = c(
              "Before-After-Control-Impact assessment" = "baci",
              "Control-Impact assessment"              = "ci"
            ),
            selected = if (!is.null(design)) design else "baci"
          )
        ),
        div(class = "section-divider"),
        if (!is.null(design) && design == "baci") {
          tagList(
            make_source_slot_ui("before", 'Source type "before"'),
            div(class = "section-divider"),
            make_source_slot_ui("after",  'Source type "after"')
          )
        } else if (!is.null(design) && design == "ci") {
          make_source_slot_ui("effect", 'Source type "effect"')
        } else {
          NULL
        },

        # Spatial unit of assessment
        div(class = "section-divider"),
        radioButtons("baci_spatial_unit",
          label   = "Spatial unit of impact assessment",
          choices = c(
            "Individual unit of analysis (vector feature)" = "individual",
            "Pooled units of analysis"                     = "pooled"
          ),
          selected = "individual"
        ),

        # Run button
        div(style = "margin-top:12px;",
          actionButton("run_baci", "Run impact assessment",
            class = "btn btn-success btn-lg", width = "100%")
        ),

        # Result status
        uiOutput("baci_run_status_ui")
      )
    } else {
      NULL
    }
  })

  # =========================================================================
  # -- Tab 6: Run impact assessment -----------------------------------------
  # =========================================================================

  impact_result <- reactiveVal(NULL)

  observeEvent(input$run_baci, {

    v          <- baci_vect_data()
    treat_col     <- input$baci_col_treatment
    uid_col       <- input$baci_col_uid
    match_ids_col <- input$baci_col_match_ids
    treat_val     <- input$baci_treat_value
    design     <- input$baci_design
    spat_unit  <- input$baci_spatial_unit
    method     <- input$baci_method

    if (is.null(v)) {
      showNotification("No matched pairs vector loaded.", type = "error")
      return()
    }
    if (is.null(treat_col) || nchar(treat_col) == 0) {
      showNotification("No treatment attribute selected.", type = "error")
      return()
    }

    if (is.null(treat_col) || nchar(treat_col) == 0) {
      showNotification("No treatment attribute selected.", type = "error")
      return()
    }
    match_id_col <- if (!is.null(match_ids_col) && nchar(match_ids_col) > 0) {
      match_ids_col
    } else { NULL }
    if (is.null(match_id_col) && spat_unit == "individual") {
      showNotification(
        "No match ID column selected. Select the .match_ids column or use pooled analysis.",
        type = "error")
      return()
    }

    # -- Build sources list from UI inputs ----------------------------------
    build_source_spec <- function(slot_id) {
      src_type <- input[[paste0("ie_srctype_", slot_id)]]
      if (is.null(src_type) || nchar(src_type) == 0) return(NULL)

      if (src_type == "from_vector") {
        attr_sel <- input[[paste0("ie_attr_", slot_id)]]
        if (is.null(attr_sel) || length(attr_sel) == 0) return(NULL)

        # For before/after with multiple attrs, compute canonical base names.
        # If names don't match by suffix stripping, pair by selection order
        # and rename the "after" attrs to match "before" base names.
        rename_map <- NULL
        if (slot_id == "after" && length(attr_sel) > 1) {
          before_sel <- input[["ie_attr_before"]]
          if (!is.null(before_sel) && length(before_sel) == length(attr_sel)) {
            strip <- function(x, sfx) sub(paste0("_", sfx, "$"), "", x)
            b_base <- strip(before_sel, "before")
            a_base <- strip(attr_sel,   "after")
            if (!identical(sort(b_base), sort(a_base))) {
              # Names don't match: pair by order, rename after -> before base names
              rename_map <- setNames(b_base, attr_sel)
            }
          }
        }

        return(list(
          source_type = "from_vector",
          attrs       = attr_sel,
          rename_map  = rename_map   # NULL if no rename needed
        ))
      }

      # File-based source: get from ie_source_list
      conf <- ie_source_list()[[slot_id]]
      if (is.null(conf)) {
        showNotification(
          paste0("Source '", slot_id, "' is not confirmed. Please click Confirm."),
          type = "warning")
        return(NULL)
      }

      # Read live UI values for fun_custom/fraction/na_rm (not stored in list)
      conf$fun_custom   <- input[[paste0("ie_custom_",     slot_id)]]
      conf$fun_fraction <- input[[paste0("ie_fraction_",   slot_id)]]
      na_rm_str         <- input[[paste0("ie_narm_",       slot_id)]]
      conf$na.rm        <- !identical(na_rm_str, "FALSE")
      conf$use_chunks   <- isTRUE(input[[paste0("ie_use_chunks_", slot_id)]])
      chunk_val         <- input[[paste0("ie_chunk_size_", slot_id)]]
      conf$chunk_size   <- if (!is.null(chunk_val) && !is.na(chunk_val)) {
        as.integer(chunk_val)
      } else { 500L }
      conf$layer_prefix <- tools::file_path_sans_ext(conf$name)
      conf
    }

    sources <- list()
    if (design == "baci") {
      sources$before <- build_source_spec("before")
      sources$after  <- build_source_spec("after")
      if (is.null(sources$before) || is.null(sources$after)) return()
    } else {
      sources$effect <- build_source_spec("effect")
      if (is.null(sources$effect)) return()
    }

    # -- Run ----------------------------------------------------------------
    progress <- shiny::Progress$new()
    progress$set(message = "Running impact assessment...", value = 0.1)
    on.exit(progress$close())

    shiny_progress <- function(msg, frac) {
      progress$set(value = frac, message = msg)
    }

    result <- tryCatch(
      run_impact_assessment(
        matched_vect  = v,
        sources       = sources,
        col_treatment = treat_col,
        treat_value   = treat_val,
        col_uid       = if (!is.null(uid_col) && nchar(uid_col) > 0) uid_col else ".uid",
        col_match_ids = if (!is.null(match_id_col)) match_id_col else ".match_ids",
        design        = design,
        spatial_unit  = spat_unit,
        progress_fun  = shiny_progress
      ),
      error = function(e) {
        list(result_vect = NULL, result_df = NULL,
             effect_cols = character(0),
             errors = paste0("Fatal error: ", conditionMessage(e)))
      }
    )

    # Merge BACI results back into the full matched vector (all units)
    if (!is.null(result$result_vect)) {
      tryCatch({
        v_df     <- as.data.frame(v)
        res_df   <- as.data.frame(result$result_vect)
        uid_col  <- if (!is.null(uid_col) && nchar(uid_col) > 0) uid_col else ".uid"
        new_cols <- setdiff(names(res_df), names(v_df))
        if (length(new_cols) > 0 && uid_col %in% names(v_df) && uid_col %in% names(res_df)) {
          merged_df  <- merge(v_df, res_df[, c(uid_col, new_cols), drop = FALSE],
                              by = uid_col, all.x = TRUE, sort = FALSE)
          # Re-align to original row order
          merged_df  <- merged_df[match(v_df[[uid_col]], merged_df[[uid_col]]), , drop = FALSE]
          rownames(merged_df) <- NULL
          geom_wkt   <- terra::geom(v, wkt = TRUE)
          for (cn in names(merged_df)) {
            if (is.factor(merged_df[[cn]])) merged_df[[cn]] <- as.character(merged_df[[cn]])
          }
          merged_vect <- terra::vect(cbind(merged_df, geometry = geom_wkt),
                                     geom = "geometry", crs = terra::crs(v))
          result$merged_vect <- merged_vect
        }
      }, error = function(e) {
        result$errors <- c(result$errors,
          paste0("Could not merge results into input vector: ", conditionMessage(e)))
      })
    }

    impact_result(result)

    if (length(result$errors) > 0) {
      showNotification(paste(result$errors, collapse = "
"),
                       type = "warning", duration = 15)
    }

    if (!is.null(result$result_vect) || !is.null(result$result_df)) {
      showNotification("Impact assessment complete.", type = "message")
    }

  }, ignoreInit = TRUE)

  # Track which impact unit is selected on the results map
  baci_selected_id <- reactiveVal(NULL)
  baci_shape_just_clicked <- reactiveVal(FALSE)

  # Status summary + save option below Run button
  output$baci_run_status_ui <- renderUI({
    res <- impact_result()
    if (is.null(res)) return(NULL)

    err_items <- if (!is.null(res$errors) && length(res$errors) > 0) {
      lapply(res$errors, function(e) {
        p(style = "color:#c0392b; font-size:12px; margin:2px 0;",
          paste0("! ", e))
      })
    } else { list() }

    ok_msg <- if (!is.null(res$result_vect)) {
      p(style = "color:#1e8449; font-size:13px; font-weight:600; margin-top:8px;",
        paste0("v Complete: ", nrow(res$result_vect), " impact unit(s), ",
               length(res$effect_cols), " effect variable(s)."))
    } else if (!is.null(res$result_df)) {
      p(style = "color:#1e8449; font-size:13px; font-weight:600; margin-top:8px;",
        paste0("v Pooled analysis complete: ",
               length(res$effect_cols), " effect variable(s)."))
    } else { NULL }

    tagList(ok_msg, do.call(tagList, err_items))
  })

  observeEvent(input$baci_save_btn, {
    res  <- impact_result()
    path <- trimws(input$baci_output_filename)
    if (is.null(res) || is.null(res$result_vect)) return()
    out_path <- if (nchar(path) > 0) paste0(path, ".gpkg") else {
      paste0("impact_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".gpkg")
    }
    tryCatch({
      terra::writeVector(res$result_vect, out_path, overwrite = TRUE)
      output$baci_save_status_ui <- renderUI(
        p(style = "color:#1e8449; font-size:12px; margin-top:4px;",
          paste0("v Saved: ", out_path))
      )
    }, error = function(e) {
      output$baci_save_status_ui <- renderUI(
        p(style = "color:#c0392b; font-size:12px; margin-top:4px;",
          paste0("! ", conditionMessage(e)))
      )
    })
  }, ignoreInit = TRUE)

  # =========================================================================
  # -- Impact evaluation results card (inline, below method card) -----------
  # =========================================================================

  # sf reactive for the impact results
  # ---------------------------------------------------------------------------
  # Smart number formatting for small/large contrast values
  # ---------------------------------------------------------------------------

  # Returns the number of decimal places needed for at least 2 sig figs,
  # capped at 4 before switching to scientific notation.
  baci_needs_sci <- function(vals) {
    finite_vals <- vals[is.finite(vals)]
    if (length(finite_vals) == 0) return(FALSE)
    abs_max <- max(abs(finite_vals), na.rm = TRUE)
    if (abs_max == 0) return(FALSE)
    # Need sci notation if max absolute value < 0.001 (would show 0.0000 at 4dp)
    abs_max < 0.001
  }

  # Format a numeric vector for display: scientific if needed, else 4 dp
  baci_format_val <- function(x, sci = NULL) {
    if (is.null(sci)) { sci <- baci_needs_sci(x) }
    if (sci) {
      formatC(x, format = "e", digits = 2)
    } else {
      formatC(x, format = "f", digits = 4)
    }
  }

  # leaflet labFormat using smart notation.
  # leaflet::labelFormat(transform=) feeds result into formatNum() which
  # requires numerics. Instead we return a raw label function directly,
  # matching the signature leaflet expects: function(type, cuts, p, p_round).
  baci_lab_format <- function(vals) {
    sci <- baci_needs_sci(vals)
    function(type, cuts, p, p_round, ...) {
      formatted <- if (sci) {
        formatC(cuts, format = "e", digits = 2)
      } else {
        formatC(cuts, format = "f", digits = 4)
      }
      paste0(formatted)
    }
  }

  # reactiveVal: sf of impact results, set once when impact_result() changes.
  # Using reactiveVal instead of reactive() means nothing re-computes this
  # when other inputs (baci_map_var etc.) change - breaking the reset chain.
  baci_result_sf_val <- reactiveVal(NULL)

  observeEvent(impact_result(), {
    res <- impact_result()
    if (is.null(res) || is.null(res$result_vect)) {
      baci_result_sf_val(NULL)
      return()
    }
    sf_obj  <- to_leaflet_sf(res$result_vect)
    uid_col <- isolate(input$baci_col_uid)
    if (!is.null(uid_col) && nchar(uid_col) > 0 && uid_col %in% names(sf_obj)) {
      sf_obj$.feature_id <- as.character(sf_obj[[uid_col]])
    } else {
      sf_obj$.feature_id <- as.character(seq_len(nrow(sf_obj)))
    }
    baci_result_sf_val(sf_obj)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Output flag for conditionalPanel in ui.R
  output$baci_results_ready <- reactive({
    res <- impact_result()
    !is.null(res) && (!is.null(res$result_vect) || !is.null(res$result_df))
  })
  outputOptions(output, "baci_results_ready", suspendWhenHidden = FALSE)

  # Summary text: uses only impact_result(), not baci_result_sf_val()
  # to avoid shared reactive dependencies that could cause cascading invalidation
  output$baci_results_summary_ui <- renderUI({
    res <- impact_result()
    if (is.null(res) || is.null(res$result_vect)) return(NULL)
    n_impact    <- nrow(res$result_vect)
    effect_cols <- res$effect_cols
    p(style = "font-size:12px; color:#888; margin-bottom:8px;",
      paste0(n_impact, " impact unit(s), ",
             length(effect_cols), " effect variable(s). ",
             "Click a unit to see its BACI results."))
  })

  # Populate selector when results arrive (updateSelectInput never re-renders DOM)
  observeEvent(impact_result(), {
    res <- impact_result()
    if (is.null(res) || is.null(res$result_vect)) return()
    sf_obj          <- baci_result_sf_val()
    effect_cols     <- res$effect_cols
    contrast_choices <- paste0(effect_cols, "_contrast")
    contrast_choices <- contrast_choices[contrast_choices %in% names(sf_obj)]
    updateSelectInput(session, "baci_map_var",
      choices  = setNames(contrast_choices, contrast_choices),
      selected = contrast_choices[1])
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Pooled results card
  output$baci_pooled_card_ui <- renderUI({
    res <- impact_result()
    if (is.null(res) || !is.null(res$result_vect)) return(NULL)
    if (is.null(res$result_df)) return(NULL)
    div(class = "card",
      div(class = "card-title",
        span(class = "icon", "\U0001f4ca"),
        "Impact assessment results (pooled)"),
      div(style = "overflow-x:auto;", tableOutput("baci_pooled_table"))
    )
  })

  output$baci_pooled_table <- renderTable({
    res <- impact_result()
    req(!is.null(res) && !is.null(res$result_df))
    as.data.frame(res$result_df)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 4)

  # Helper: compute fill colours and opacity for the results map.
  # baci_map_colours: derives colours from baci_result_sf_val() - the same
  # object used by leafletProxy - so cm$cols and cm$fill_op are always in
  # the same row order as the features being rendered.
  baci_map_colours <- reactive({
    sf_obj <- baci_result_sf_val()
    res    <- impact_result()
    req(!is.null(sf_obj) && !is.null(res))
    df     <- sf::st_drop_geometry(sf_obj)

    contrast_col <- if (!is.null(input$baci_map_var) &&
                        nchar(input$baci_map_var) > 0 &&
                        input$baci_map_var %in% names(df)) {
      input$baci_map_var
    } else {
      paste0(res$effect_cols[1], "_contrast")
    }
    if (!contrast_col %in% names(df)) return(NULL)

    vals    <- df[[contrast_col]]
    abs_max <- max(abs(range(vals, na.rm = TRUE)), na.rm = TRUE)
    if (abs_max == 0) abs_max <- 1
    pal  <- leaflet::colorNumeric(
      palette = c("#2980b9","white","#c0392b"),
      domain  = c(-abs_max, abs_max), na.color = "#aaaaaa")
    cols <- pal(vals)

    # Significance greying
    grey_nonsig <- isTRUE(input$baci_grey_nonsig)
    pval_col    <- sub("_contrast$", "_pvalue", contrast_col)
    fill_op <- if (grey_nonsig && pval_col %in% names(df)) {
      pvals <- df[[pval_col]]
      ifelse(is.na(pvals) | pvals > 0.05, 0.12, 0.85)
    } else {
      rep(0.85, nrow(df))
    }

    list(cols = cols, fill_op = fill_op, pal = pal,
         vals = vals, contrast_col = contrast_col)
  })

  # Results map: initial render.
  # Uses isolate() for colours so re-renders ONLY when impact_result() changes
  # (i.e. a new analysis is run). Variable/significance changes and click
  # highlighting are handled exclusively via leafletProxy in the observe() below.
  output$baci_result_map <- renderLeaflet({
    sf_obj <- baci_result_sf_val()
    req(!is.null(sf_obj) && nrow(sf_obj) > 0)
    # Initial render uses first contrast variable only.
    # Subsequent variable/significance changes go through observe+leafletProxy.
    res          <- isolate(impact_result())
    ec           <- res$effect_cols[1]
    contrast_col <- paste0(ec, "_contrast")
    df           <- sf::st_drop_geometry(sf_obj)
    vals         <- if (contrast_col %in% names(df)) df[[contrast_col]] else rep(0, nrow(df))
    abs_max      <- max(abs(range(vals, na.rm = TRUE)), na.rm = TRUE)
    if (abs_max == 0) { abs_max <- 1 }
    pal <- leaflet::colorNumeric(
      palette = c("#2980b9","white","#c0392b"),
      domain  = c(-abs_max, abs_max), na.color = "#aaaaaa")
    cm <- list(cols = pal(vals), fill_op = rep(0.8, nrow(df)),
               pal = pal, vals = vals, contrast_col = contrast_col)
    req(!is.null(cm))
    geom_t <- unique(sf::st_geometry_type(sf_obj))
    is_pt  <- any(geom_t %in% c("POINT","MULTIPOINT"))

    m <- leaflet::leaflet(sf_obj) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
        options = leaflet::tileOptions(opacity = 0.5))

    if (is_pt) {
      m <- m |> leaflet::addCircleMarkers(
        layerId = ~.feature_id,
        fillColor = cm$cols, color = "white",
        fillOpacity = cm$fill_op, opacity = 1,
        radius = 7, weight = 1.5)
    } else {
      m <- m |> leaflet::addPolygons(
        layerId = ~.feature_id,
        fillColor = cm$cols, fillOpacity = cm$fill_op,
        color = "white", weight = 1, smoothFactor = 0.5,
        options = leaflet::pathOptions(clickable = TRUE))
    }
    m |> leaflet::addLegend(
      layerId   = "baci_legend",
      position  = "bottomright",
      pal       = cm$pal, values = cm$vals,
      title     = cm$contrast_col, opacity = 0.8,
      labFormat = baci_lab_format(cm$vals))
  })

  # Click: select impact unit
  observeEvent(input$baci_result_map_shape_click, {
    baci_shape_just_clicked(TRUE)
    baci_selected_id(input$baci_result_map_shape_click$id)
  }, ignoreInit = TRUE)

  observeEvent(input$baci_result_map_click, {
    if (isTRUE(baci_shape_just_clicked())) {
      baci_shape_just_clicked(FALSE)
    } else {
      baci_selected_id(NULL)
    }
  }, ignoreInit = TRUE)

  # Update map colours, legend, and selection highlight via leafletProxy.
  # Fires when: variable selector changes, significance checkbox changes,
  # or a unit is clicked/deselected.
  observe({
    sel    <- baci_selected_id()
    sf_obj <- baci_result_sf_val()
    res    <- impact_result()
    if (is.null(res) || is.null(res$result_vect) || is.null(sf_obj)) return()
    cm     <- baci_map_colours()
    if (is.null(cm)) return()
    # df derived from same sf_obj as cm -- row order is guaranteed consistent
    df     <- sf::st_drop_geometry(sf_obj)
    geom_t <- unique(sf::st_geometry_type(sf_obj))
    is_pt  <- any(geom_t %in% c("POINT","MULTIPOINT"))

    has_sel  <- !is.null(sel) && any(df$.feature_id == sel)
    is_sel   <- if (has_sel) { df$.feature_id == sel } else { rep(FALSE, nrow(df)) }

    # Combine significance greying with selection dimming:
    # selected unit -> full opacity, others -> dimmed if something selected
    base_op  <- cm$fill_op   # already accounts for significance greying
    fill_op  <- if (has_sel) {
      ifelse(is_sel, 1, pmin(base_op, 0.2))
    } else {
      base_op
    }
    stroke_c <- ifelse(is_sel, "black", "white")
    stroke_w <- ifelse(is_sel, 3, 1)

    proxy <- leaflet::leafletProxy("baci_result_map", data = sf_obj)

    # Update shapes
    if (is_pt) {
      proxy |> leaflet::clearMarkers() |>
        leaflet::addCircleMarkers(
          layerId = ~.feature_id,
          fillColor = cm$cols, color = stroke_c,
          fillOpacity = fill_op,
          radius = ifelse(is_sel, 10, 7),
          weight = stroke_w, stroke = TRUE)
    } else {
      proxy |> leaflet::clearShapes() |>
        leaflet::addPolygons(
          layerId = ~.feature_id,
          fillColor = cm$cols, fillOpacity = fill_op,
          color = stroke_c, weight = stroke_w, smoothFactor = 0.5,
          options = leaflet::pathOptions(clickable = TRUE))
    }

    # Update legend: remove old and add new with current variable and palette
    proxy |>
      leaflet::removeControl("baci_legend") |>
      leaflet::addLegend(
        layerId   = "baci_legend",
        position  = "bottomright",
        pal       = cm$pal,
        values    = cm$vals,
        title     = cm$contrast_col,
        opacity   = 0.8,
        labFormat = baci_lab_format(cm$vals))
  })

  # Detail panel for selected unit
  output$baci_result_detail_ui <- renderUI({
    sel <- baci_selected_id()
    if (is.null(sel)) return(NULL)
    sf_obj      <- baci_result_sf_val()
    res         <- impact_result()
    df          <- sf::st_drop_geometry(sf_obj)
    row         <- df[df$.feature_id == sel, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    effect_cols <- res$effect_cols

    uid_col <- isolate(input$baci_col_uid)
    uid_val <- if (!is.null(uid_col) && uid_col %in% names(row)) {
      as.character(row[[uid_col]])
    } else { sel }

    # Build results table for this unit
    result_rows <- lapply(effect_cols, function(ec) {
      contrast_col <- paste0(ec, "_contrast")
      pval_col     <- paste0(ec, "_pvalue")
      data.frame(
        Variable = ec,
        Contrast = if (contrast_col %in% names(row)) round(row[[contrast_col]], 4) else NA,
        P.value  = if (pval_col %in% names(row)) {
          pv <- row[[pval_col]]
          if (is.na(pv)) "NA" else formatC(pv, format = "g", digits = 3)
        } else { "NA" },
        stringsAsFactors = FALSE
      )
    })
    result_df <- do.call(rbind, result_rows)

    div(style = "margin-top:12px; padding:12px; background:#f7f9fb; border-radius:6px;",
      strong(style = "font-size:13px;", paste0("Unit: ", uid_val)),
      div(style = "overflow-x:auto; margin-top:8px;",
        tableOutput("baci_unit_detail_table")
      )
    )
  })

  # Save card: appears below results (mirrors matching analysis save card)
  output$baci_save_card_ui <- renderUI({
    res <- impact_result()
    if (is.null(res) || is.null(res$result_vect)) return(NULL)
    div(class = "card",
      div(class = "card-title",
        span(class = "icon", "\U0001f4be"),
        "Save results"
      ),
      p(style = "font-size:13px; color:#666; margin-bottom:16px;",
        "Save the impact assessment results (impact units with BACI contrast
         and p-values) to a GeoPackage file."),
      div(style = "display:flex; align-items:flex-end; gap:10px;",
        div(style = "flex:1;",
          textInput("baci_output_filename",
            label       = "Output file path (without extension)",
            placeholder = "e.g. C:/Data/impact_results",
            width       = "100%")
        ),
        div(style = "margin-bottom:15px;",
          actionButton("baci_save_btn", "Save .gpkg",
            class = "btn btn-success btn-sm")
        )
      ),
      uiOutput("baci_save_status_ui")
    )
  })

  output$baci_unit_detail_table <- renderTable({
    sel <- baci_selected_id()
    req(!is.null(sel))
    sf_obj      <- baci_result_sf_val()
    res         <- impact_result()
    df          <- sf::st_drop_geometry(sf_obj)
    row         <- df[df$.feature_id == sel, , drop = FALSE]
    effect_cols <- res$effect_cols

    # Collect all contrast values to decide formatting once for the whole table
    all_contrasts <- unlist(lapply(effect_cols, function(ec) {
      cn <- paste0(ec, "_contrast")
      if (cn %in% names(row)) row[[cn]] else NA_real_
    }))
    sci <- baci_needs_sci(all_contrasts)

    result_rows <- lapply(effect_cols, function(ec) {
      contrast_val <- if (paste0(ec,"_contrast") %in% names(row)) {
        row[[paste0(ec,"_contrast")]]
      } else { NA_real_ }
      pval_val <- if (paste0(ec,"_pvalue") %in% names(row)) {
        row[[paste0(ec,"_pvalue")]]
      } else { NA_real_ }
      data.frame(
        Variable = ec,
        Contrast = baci_format_val(contrast_val, sci = sci),
        P.value  = if (is.na(pval_val)) {
          "NA"
        } else {
          formatC(pval_val, format = "g", digits = 3)
        },
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, result_rows)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "NA")
