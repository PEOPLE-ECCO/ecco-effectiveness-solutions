server <- function(input, output, session) {
  
  # Null coalescing helper
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # -- Vector: load ------------------------------------------------------------
  
  vect_data <- reactive({
    req(input$geojson_file)
    paths    <- input$geojson_file$datapath
    names_up <- input$geojson_file$name
    new_paths <- file.path(dirname(paths), names_up)
    file.rename(paths, new_paths)
    entry <- if (any(grepl("\\.shp$", names_up, ignore.case = TRUE))) {
      new_paths[grepl("\\.shp$", names_up, ignore.case = TRUE)]
    } else {
      new_paths[1]
    }
    tryCatch(terra::vect(entry), error = function(e) {NULL})
  })
  
  # -- Vector: geometry info ----------------------------------------------------
  
  geom_info <- reactive({
    v <- vect_data()
    req(!is.null(v))
    raw_types <- unique(terra::geomtype(v))
    geom_lookup <- c(
      polygons = "Polygon", polygon = "Polygon",
      lines    = "Lines",   line    = "Lines",
      points   = "Points",  point   = "Points"
    )
    canonical <- vapply(raw_types, function(t) {
      lbl <- geom_lookup[t]
      if (is.na(lbl)) { t } else { unname(lbl) }
    }, character(1))
    list(
      types  = unname(canonical),
      n_feat = nrow(v),
      crs    = terra::crs(v, describe = TRUE)$name
    )
  })
  
  # -- Vector: geometry badge ---------------------------------------------------
  
  output$geom_type_ui <- renderUI({
    if (is.null(input$geojson_file)) {
      return(p(class = "placeholder-msg", "Geometry information will appear here after upload."))
    }
    v <- vect_data()
    if (is.null(v)) {
      return(div(class = "geom-badge geom-error", "\u26a0", "Could not read file."))
    }
    info <- geom_info()
    if (length(info$types) > 1) {
      return(div(class = "geom-badge geom-error", "\u26a0",
                 paste0("Multiple geometry types: ", paste(info$types, collapse = ", "), ".")))
    }
    type        <- info$types
    badge_class <- switch(type,
                          Polygon = "geom-badge geom-polygon",
                          Lines   = "geom-badge geom-lines",
                          Points  = "geom-badge geom-points",
                          "geom-badge geom-error"
    )
    icon_chr <- switch(type, Polygon = "\u2b21", Lines = "\u3030", Points = "\u25cf", "?")
    crs_name <- info$crs
    crs_pill <- if (!is.na(crs_name) && nchar(crs_name) > 0) {
      span(class = "info-pill", paste0("CRS: ", crs_name))
    } else {
      NULL
    }
    tagList(
      div(class = "section-divider"),
      div(style = "margin-bottom:10px;",
          strong(style = "font-size:13px; color:#555;", "Detected geometry type:")),
      div(class = badge_class, icon_chr, type),
      div(class = "info-row",
          span(class = "info-pill", paste0("Features: ", info$n_feat)),
          crs_pill
      )
    )
  })
  
  # -- Vector: treatment attribute card ----------------------------------------
  
  output$select_ci_card_ui <- renderUI({
    v <- vect_data()
    if (is.null(v)) return(NULL)
    info <- geom_info()
    if (length(info$types) > 1) return(NULL)
    attr_names <- names(v)
    if (length(attr_names) == 0) {
      return(div(class = "card",
                 div(class = "card-title", span(class = "icon", "\U0001f6ab"), "Treatment attribute"),
                 p(style = "color:#e67e22; font-size:13px;",
                   "\u26a0 No attributes found.")
      ))
    }
    div(class = "card",
        div(class = "card-title", span(class = "icon", "\U0001f6ab"), "Treatment attribute"),
        p(style = "font-size:13px; color:#666; margin-bottom:16px;",
          "Select the attribute that identifies the treatment (impact/control)."),
        selectInput("ci_field", label = "Treatment attribute",
                    choices = setNames(attr_names, attr_names), selected = NULL, width = "360px")
    )
  })
  
  # -- Vector: unique ID card ---------------------------------------------------
  
  output$attr_card_ui <- renderUI({
    v <- vect_data()
    if (is.null(v)) return(NULL)
    info <- geom_info()
    if (length(info$types) > 1) return(NULL)
    attr_names <- names(v)
    if (length(attr_names) == 0) {
      return(div(class = "card",
                 div(class = "card-title", span(class = "icon", "\U0001f511"), "Unique Feature ID"),
                 p(style = "color:#e67e22; font-size:13px;", "\u26a0 No attributes found.")
      ))
    }
    div(class = "card",
        div(class = "card-title", span(class = "icon", "\U0001f511"), "Unique Feature ID"),
        div(style = "display:flex; align-items:center; gap:10px; margin-bottom:16px;",
            checkboxInput(inputId = "use_uid", label = NULL, value = FALSE),
            p(style = "font-size:13px; color:#666; margin:0;",
              "Select the attribute that contains the unique identifier for each feature.")
        ),
        conditionalPanel(
          condition = "input.use_uid == true",
          selectInput("uid_field", label = "Unique ID attribute",
                      choices = setNames(attr_names, attr_names), selected = NULL, width = "360px"),
          uiOutput("uid_preview_ui")
        )
    )
  })
  
  # -- Vector: UID preview ------------------------------------------------------
  
  output$uid_preview_ui <- renderUI({
    req(input$uid_field, vect_data())
    v        <- vect_data()
    col      <- input$uid_field
    vals     <- as.character(as.data.frame(v)[[col]])
    n_unique <- length(unique(vals))
    n_total  <- length(vals)
    is_unique <- (n_unique == n_total)
    flag_col  <- if (is_unique) { "#1e8449" } else { "#c0392b" }
    flag_icon <- if (is_unique) { "\u2714" } else { "\u26a0" }
    flag_msg  <- if (is_unique) {
      paste0("All ", n_total, " values are unique \u2014 good ID field.")
    } else {
      paste0(n_unique, " unique values across ", n_total, " features \u2014 not fully unique.")
    }
    preview_str <- paste(head(vals, 5), collapse = ", ")
    sfx <- if (n_total > 5) { paste0(" (+", n_total - 5, " more)") } else { "" }
    tagList(
      div(class = "section-divider"),
      div(class = "info-row",
          span(style = paste0("color:", flag_col, "; font-weight:600; font-size:13px;"),
               paste(flag_icon, flag_msg))
      ),
      div(class = "info-row", style = "margin-top:6px;",
          span(class = "info-pill", paste0("Preview: ", preview_str, sfx))
      )
    )
  })
  
  # -- Vector: plot card --------------------------------------------------------
  
  output$vector_plot_card_ui <- renderUI({
    v <- vect_data()
    if (is.null(v)) return(NULL)
    info <- geom_info()
    if (length(info$types) > 1) return(NULL)
    div(class = "card", style = "height:100%;",
        div(class = "card-title", span(class = "icon", "\U0001f5fa"), "Vector Preview"),
        plotOutput("vector_plot", height = "560px")
    )
  })
  
  output$vector_plot <- renderPlot({
    v <- vect_data()
    req(!is.null(v))
    use_ci  <- !is.null(input$ci_field)  && nchar(input$ci_field)  > 0 && input$ci_field  %in% names(v)
    use_uid <- isTRUE(input$use_uid) && !is.null(input$uid_field) && nchar(input$uid_field) > 0 && input$uid_field %in% names(v)
    if (use_ci) {
      terra::plot(v, input$ci_field,  main = input$ci_field)
    } else if (use_uid) {
      terra::plot(v, input$uid_field, main = input$uid_field)
    } else if (length(names(v)) > 0) {
      terra::plot(v, names(v)[1],     main = names(v)[1])
    } else {
      terra::plot(v)
    }
  }, res = 96, bg = "white")
  
  # -- Matching covariates: Card 1 (attributes from input vector) -------------
  
  output$matchvars_select_ui <- renderUI({
    v <- vect_data()
    if (is.null(v)) {
      return(p(class = "placeholder-msg",
               "Load a vector file in the Vector Input tab first."))
    }
    attr_names <- names(v)
    if (length(attr_names) == 0) {
      return(p(style = "color:#e67e22; font-size:13px;",
               "No attributes found in the vector file."))
    }
    selectInput(
      inputId  = "col_matchvars",
      label    = "Select matching variables:",
      choices  = setNames(attr_names, attr_names),
      selected = NULL,
      multiple = TRUE,
      width    = "480px"
    )
  })
  
  # -- Matching covariates: Card 2 (external sources, iterative) ---------------
  
  matchlyr_list <- reactiveVal(list())
  
  validate_local_raster <- function(path) {
    path <- trimws(path)
    if (nchar(path) == 0) return(list(ok = FALSE, msg = ""))
    if (!file.exists(path)) return(list(ok = FALSE, msg = "File not found."))
    if (!grepl("[.]tiff?$", path, ignore.case = TRUE)) {
      return(list(ok = FALSE, msg = "Not a .tif file. Please provide a GeoTIFF."))
    }
    r <- tryCatch(terra::rast(path), error = function(e) {NULL})
    if (is.null(r)) return(list(ok = FALSE, msg = "Could not open file with terra."))
    nlyr      <- terra::nlyr(r)
    idx       <- seq_len(nlyr)
    raw_names <- names(r)
    if (length(raw_names) != nlyr || all(nchar(trimws(raw_names)) == 0)) {
      raw_names <- paste0("Layer_", idx)
    }
    lyr_choices <- setNames(as.character(idx), paste0(idx, ": ", raw_names))
    list(
      ok          = TRUE,
      source_type = "Local raster",
      name        = basename(path),
      path        = path,
      nrow        = terra::nrow(r),
      ncol        = terra::ncol(r),
      nlyr        = nlyr,
      res         = round(terra::res(r), 4),
      crs         = terra::crs(r, describe = TRUE)$name,
      lyr_choices = lyr_choices,
      resamp      = "bilinear",
      sel_lyr     = as.character(idx),
      fun          = "mean",
      na_rm        = "TRUE",
      fun_custom   = "",
      fun_fraction = ""
    )
  }
  
  validate_url_raster <- function(url) {
    url <- trimws(url)
    if (nchar(url) == 0) return(list(ok = FALSE, msg = ""))
    if (!grepl("^https?://", url, ignore.case = TRUE)) {
      return(list(ok = FALSE, msg = "URL must start with http:// or https://"))
    }
    # Build a GDAL virtual path and open header-only with terra
    vsi_path <- paste0("/vsicurl/", url)
    r <- tryCatch(terra::rast(vsi_path), error = function(e) {NULL})
    if (is.null(r)) {
      return(list(ok = FALSE, msg = "Could not open URL with terra. Check the URL points to a valid GeoTIFF."))
    }
    nlyr      <- terra::nlyr(r)
    idx       <- seq_len(nlyr)
    raw_names <- names(r)
    if (length(raw_names) != nlyr || all(nchar(trimws(raw_names)) == 0)) {
      raw_names <- paste0("Layer_", idx)
    }
    lyr_choices <- setNames(as.character(idx), paste0(idx, ": ", raw_names))
    list(
      ok          = TRUE,
      source_type = "URL raster",
      name        = basename(url),
      path        = url,
      nrow        = terra::nrow(r),
      ncol        = terra::ncol(r),
      nlyr        = nlyr,
      res         = round(terra::res(r), 4),
      crs         = terra::crs(r, describe = TRUE)$name,
      lyr_choices = lyr_choices,
      resamp      = "bilinear",
      sel_lyr     = as.character(idx),
      fun          = "mean",
      na_rm        = "TRUE",
      fun_custom   = "",
      fun_fraction = ""
    )
  }
  
  validate_peopleecco <- function(path) {
    path <- trimws(path)
    if (nchar(path) == 0) return(list(ok = FALSE, msg = ""))
    if (!file.exists(path)) return(list(ok = FALSE, msg = "File not found."))
    r <- tryCatch(terra::rast(path), error = function(e) {NULL})
    if (is.null(r)) return(list(ok = FALSE, msg = "Could not open file with terra."))
    nlyr      <- terra::nlyr(r)
    idx       <- seq_len(nlyr)
    raw_names <- names(r)
    if (length(raw_names) != nlyr || all(nchar(trimws(raw_names)) == 0)) {
      raw_names <- paste0("Layer_", idx)
    }
    lyr_choices <- setNames(as.character(idx), paste0(idx, ": ", raw_names))
    list(
      ok          = TRUE,
      source_type = "PEOPLE-ECCO dataset",
      name        = basename(path),
      path        = path,
      nrow        = terra::nrow(r),
      ncol        = terra::ncol(r),
      nlyr        = nlyr,
      res         = round(terra::res(r), 4),
      crs         = terra::crs(r, describe = TRUE)$name,
      lyr_choices = lyr_choices,
      resamp      = "bilinear",
      sel_lyr     = as.character(idx),
      fun          = "mean",
      na_rm        = "TRUE",
      fun_custom   = "",
      fun_fraction = "",
      use_chunks   = FALSE,
      chunk_size   = 500L
    )
  }
  
  validate_local_vector <- function(path) {
    path <- trimws(path)
    if (nchar(path) == 0) return(list(ok = FALSE, msg = ""))
    if (!file.exists(path)) return(list(ok = FALSE, msg = "File not found."))
    ext <- tolower(tools::file_ext(path))
    if (!ext %in% c("shp", "geojson", "json", "gpkg")) {
      return(list(ok = FALSE, msg = "Unsupported format. Use .shp, .geojson, .gpkg."))
    }
    v <- tryCatch(terra::vect(path), error = function(e) {NULL})
    if (is.null(v)) return(list(ok = FALSE, msg = "Could not open file with terra."))
    raw_types <- unique(terra::geomtype(v))
    geom_lookup <- c(
      polygons = "Polygon", polygon = "Polygon",
      lines    = "Lines",   line    = "Lines",
      points   = "Points",  point   = "Points"
    )
    canonical <- vapply(raw_types, function(t) {
      lbl <- geom_lookup[t]
      if (is.na(lbl)) { t } else { unname(lbl) }
    }, character(1))
    attr_names  <- names(v)
    list(
      ok          = TRUE,
      source_type = "Local vector",
      name        = basename(path),
      path        = path,
      nfeat       = nrow(v),
      geom_type   = paste(unname(canonical), collapse = "/"),
      crs         = terra::crs(v, describe = TRUE)$name,
      attr_names  = attr_names,
      vec_fun     = "mean",
      vec_attrs   = attr_names        # default: all attributes selected
    )
  }
  
  output$matchlyr_rows_ui <- renderUI({
    confirmed <- matchlyr_list()
    n         <- length(confirmed)
    
    confirmed_rows <- lapply(seq_along(confirmed), function(i) {
      r       <- confirmed[[i]]
      crs_pill <- if (!is.na(r$crs) && nchar(r$crs) > 0) {
        span(class = "info-pill", paste0("CRS: ", r$crs))
      } else {
        NULL
      }
      
      # Metadata pills differ by source type
      meta_pills <- if (r$source_type %in% c("Local raster", "URL raster", "PEOPLE-ECCO dataset")) {
        band_lbl <- if (r$nlyr == 1) { "1 band" } else { paste0(r$nlyr, " bands") }
        dim_lbl  <- paste0(r$nrow, " x ", r$ncol, " px")
        res_lbl  <- paste0("res: ", r$res[1], " x ", r$res[2])
        div(class = "info-row", style = "margin-left:34px; margin-top:4px;",
            span(class = "info-pill", r$source_type),
            span(class = "info-pill", band_lbl),
            span(class = "info-pill", dim_lbl),
            span(class = "info-pill", res_lbl),
            crs_pill
        )
      } else {
        div(class = "info-row", style = "margin-left:34px; margin-top:4px;",
            span(class = "info-pill", r$source_type),
            span(class = "info-pill", paste0(r$nfeat, " features")),
            span(class = "info-pill", r$geom_type),
            crs_pill
        )
      }
      
      div(
        style = "margin-bottom:20px; padding-bottom:16px; border-bottom:1px solid #eef0f3;",
        div(style = "display:flex; align-items:center; gap:10px;",
            span(style = paste0(
              "min-width:24px; height:24px; border-radius:50%;",
              "background:#3498db; color:white; font-size:11px; font-weight:700;",
              "display:inline-flex; align-items:center; justify-content:center;"), i),
            div(style = paste0(
              "flex:1; background:#f7f9fb; border:1px solid #e0e4ea;",
              "border-radius:6px; padding:7px 12px; font-size:13px; color:#555;",
              "white-space:nowrap; overflow:hidden; text-overflow:ellipsis;"), r$path),
            span(style = "color:#1e8449; font-size:16px; font-weight:700;", "v"),
            actionButton(paste0("remove_", i), label = "x", class = "btn btn-sm",
                         style = "padding:2px 8px; font-size:12px; background:#fdedec; color:#c0392b; border:1px solid #f5b7b1; border-radius:4px;")
        ),
        meta_pills,
        div(style = "margin-left:34px; margin-top:12px;",
            {
              if (r$source_type %in% c("Local raster", "URL raster", "PEOPLE-ECCO dataset")) {
                geom     <- tryCatch(geom_info()$types, error = function(e) {"Points"})
                is_point <- length(geom) == 1 && geom == "Points"
                tagList(
                  # Row 1: method/function + layer selection
                  # Use r$fun / r$resamp (stored values) for selected=
                  # to avoid reading input[[fun_i]] here, which would cause
                  # this entire renderUI to re-run on every function change
                  div(style = "display:flex; gap:24px; align-items:flex-start; margin-bottom:8px;",
                      div(style = "min-width:200px;",
                          if (is_point) {
                            selectInput(paste0("resamp_", i), label = "Resampling method",
                                        choices  = c("bilinear", "nearest"),
                                        selected = r$resamp, width = "100%")
                          } else {
                            selectInput(paste0("fun_", i), label = "Function",
                                        choices  = c("mean", "median", "sd", "min", "max",
                                                     "sum", "simple", "bilinear", "fraction", "custom"),
                                        selected = r$fun, width = "100%")
                          }
                      ),
                      div(style = "flex:1;",
                          selectInput(paste0("sellyr_", i), label = "Layer(s) to use",
                                      choices = r$lyr_choices, selected = r$sel_lyr,
                                      multiple = TRUE, width = "100%")
                      )
                  ),
                  # Row 2: separate uiOutput so text inputs are not destroyed on re-render
                  uiOutput(paste0("raster_row2_", i)),
                  # Row 3: chunked extraction
                  div(style = "display:flex; align-items:center; gap:16px; margin-top:8px;",
                      checkboxInput(paste0("use_chunks_", i),
                                    label = "Chunked extraction",
                                    value = r$use_chunks),
                      conditionalPanel(
                        condition = paste0("input['use_chunks_", i, "'] == true"),
                        div(style = "min-width:160px;",
                            numericInput(paste0("chunk_size_", i),
                                         label = "Chunk size (features)",
                                         value = r$chunk_size,
                                         min   = 1,
                                         step  = 100,
                                         width = "100%")
                        )
                      )
                  )
                )
              } else {
                # Local vector controls
                # Use stored values only - avoids reading input[[]] here
                # which would cause full re-render on every dropdown change.
                # Sync observers keep r$vec_fun and r$vec_attrs up to date.
                needs_attrs     <- r$vec_fun %in% c("mean","median","std","min","max","sum")
                no_attr_warning <- needs_attrs &&
                  (is.null(r$vec_attrs) || length(r$vec_attrs) == 0)
                tagList(
                  # Row 1: function dropdown
                  div(style = "display:flex; gap:24px; align-items:flex-start; margin-bottom:8px;",
                      div(style = "min-width:200px;",
                          selectInput(paste0("vec_fun_", i), label = "Function",
                                      choices  = c("mean","median","sd","min","max",
                                                   "sum","count","minDistance"),
                                      selected = r$vec_fun, width = "100%")
                      ),
                      # Row 1b: attribute selector (only for numeric-summary functions)
                      if (needs_attrs) {
                        div(style = "flex:1;",
                            selectInput(paste0("vec_attrs_", i),
                                        label    = "Attribute(s) to use",
                                        choices  = r$attr_names,
                                        selected = r$vec_attrs,
                                        multiple = TRUE,
                                        width    = "100%"),
                            if (no_attr_warning) {
                              p(style = "color:#c0392b; font-size:12px; margin:2px 0 0 0;",
                                "! At least one attribute must be selected.")
                            } else {
                              NULL
                            }
                        )
                      } else {
                        NULL
                      }
                  )
                )
              }
            }
        )
      )
    })
    
    next_index   <- n + 1
    src_type_id  <- paste0("srctype_", next_index)
    next_path_id <- paste0("lyrpath_", next_index)
    next_btn_id  <- paste0("lyradd_",  next_index)
    src_type_val <- input[[src_type_id]]
    src_selected <- if (!is.null(src_type_val) && nchar(src_type_val) > 0) { src_type_val } else { "" }
    
    source_selector <- div(style = "display:flex; align-items:center; gap:10px;",
                           span(style = paste0(
                             "min-width:24px; height:24px; border-radius:50%;",
                             "background:#bdc3c7; color:white; font-size:11px; font-weight:700;",
                             "display:inline-flex; align-items:center; justify-content:center;"),
                             next_index),
                           div(style = "min-width:220px;",
                               selectInput(src_type_id, label = "Source type",
                                           choices  = c("Select..." = "", "Local raster" = "Local raster", "URL raster" = "URL raster", "PEOPLE-ECCO dataset" = "PEOPLE-ECCO dataset", "Local vector" = "Local vector"),
                                           selected = src_selected, width = "100%")
                           )
    )
    
    is_local_raster <- (!is.null(src_type_val) && src_type_val == "Local raster")
    is_url_raster   <- (!is.null(src_type_val) && src_type_val == "URL raster")
    is_local_vector <- (!is.null(src_type_val) && src_type_val == "Local vector")
    is_peopleecco   <- (!is.null(src_type_val) && src_type_val == "PEOPLE-ECCO dataset")
    path_lbl <- if (is_local_raster) {
      "Path to raster file (.tif)"
    } else if (is_url_raster) {
      "URL to raster file"
    } else if (is_peopleecco) {
      "Path to PEOPLE-ECCO dataset file"
    } else if (is_local_vector) {
      "Path to vector file (.shp, .geojson, .gpkg)"
    } else {
      ""
    }
    path_ph <- if (is_local_raster) {
      "e.g. C:/Users/you/data/covariate.tif"
    } else if (is_url_raster) {
      "e.g. https://example.com/data/covariate.tif"
    } else if (is_peopleecco) {
      "e.g. C:/Users/you/data/peopleecco_layer.tif"
    } else if (is_local_vector) {
      "e.g. C:/Users/you/data/covariate.shp"
    } else {
      ""
    }
    path_controls <- if (is_local_raster || is_url_raster || is_peopleecco || is_local_vector) {
      div(style = "margin-left:34px; margin-top:8px;",
          div(style = "display:flex; align-items:flex-end; gap:10px;",
              div(style = "flex:1;",
                  textInput(next_path_id, label = path_lbl,
                            placeholder = path_ph, width = "100%")
              ),
              div(style = "margin-bottom:15px;",
                  actionButton(next_btn_id, "Add", class = "btn btn-primary btn-sm")
              )
          ),
          uiOutput(paste0("lyrstatus_", next_index))
      )
    } else {
      NULL
    }
    
    do.call(tagList, c(confirmed_rows, list(
      div(style = "margin-top:4px;", source_selector, path_controls)
    )))
  })
  
  # Observers for each slot -- registered up front for up to max_lyrs slots
  max_lyrs <- 20
  lapply(seq_len(max_lyrs), function(i) {
    local({
      slot <- i
      
      observeEvent(input[[paste0("lyradd_", slot)]], {
        path <- trimws(input[[paste0("lyrpath_", slot)]])
        src  <- input[[paste0("srctype_",  slot)]]
        result <- if (!is.null(src) && src == "Local raster") {
          validate_local_raster(path)
        } else if (!is.null(src) && src == "URL raster") {
          validate_url_raster(path)
        } else if (!is.null(src) && src == "PEOPLE-ECCO dataset") {
          validate_peopleecco(path)
        } else if (!is.null(src) && src == "Local vector") {
          validate_local_vector(path)
        } else {
          list(ok = FALSE, msg = "Please select a source type first.")
        }
        if (result$ok) {
          current <- matchlyr_list()
          current[[length(current) + 1]] <- result
          matchlyr_list(current)
          output[[paste0("lyrstatus_", slot)]] <- renderUI(NULL)
        } else if (nchar(result$msg) > 0) {
          local({
            m <- result$msg
            output[[paste0("lyrstatus_", slot)]] <- renderUI(
              p(style = "color:#c0392b; font-size:12px; margin:4px 0 0 0;",
                paste0("! ", m))
            )
          })
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input[[paste0("remove_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]] <- NULL
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input[[paste0("resamp_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]]$resamp <- input[[paste0("resamp_", slot)]]
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input[[paste0("sellyr_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]]$sel_lyr <- input[[paste0("sellyr_", slot)]]
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input[[paste0("fun_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]]$fun <- input[[paste0("fun_", slot)]]
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input[[paste0("na_rm_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]]$na_rm <- input[[paste0("na_rm_", slot)]]
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      # fun_custom and fun_fraction are NOT synced on keystroke.
      # They are read directly from input[[]] at extraction time.
      
      observeEvent(input[[paste0("vec_fun_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]]$vec_fun <- input[[paste0("vec_fun_", slot)]]
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input[[paste0("vec_attrs_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]]$vec_attrs <- input[[paste0("vec_attrs_", slot)]]
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input[[paste0("use_chunks_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]]$use_chunks <- isTRUE(input[[paste0("use_chunks_", slot)]])
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input[[paste0("chunk_size_", slot)]], {
        current <- matchlyr_list()
        val <- input[[paste0("chunk_size_", slot)]]
        if (slot <= length(current) && !is.null(val) && !is.na(val) && val >= 1) {
          current[[slot]]$chunk_size <- as.integer(val)
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  })
  
  # -- Separate renderUI for raster Row 2 (avoids keystroke re-render bug) ------
  # These observers watch only fun_ changes, so text inputs are stable.
  lapply(seq_len(20), function(i) {
    local({
      slot <- i
      output[[paste0("raster_row2_", slot)]] <- renderUI({
        # Only reactive dependency here is fun_ and na_rm_ (via matchlyr_list isolate).
        # matchlyr_list() is isolated so text input changes don't re-render this.
        fun_val <- input[[paste0("fun_", slot)]]
        if (is.null(fun_val)) return(NULL)
        
        geom     <- tryCatch(geom_info()$types, error = function(e) {"Points"})
        is_point <- length(geom) == 1 && geom == "Points"
        if (is_point) return(NULL)
        
        # Read na_rm from stored list without making matchlyr_list a dependency
        stored <- isolate(matchlyr_list())
        na_rm_val <- if (slot <= length(stored)) { stored[[slot]]$na_rm } else { "TRUE" }
        
        if (fun_val == "custom") {
          textInput(paste0("fun_custom_", slot),
                    label       = "Custom function",
                    placeholder = "e.g. function(x) mean(x, na.rm=TRUE)",
                    width       = "100%")
        } else if (fun_val == "fraction") {
          textInput(paste0("fun_fraction_", slot),
                    label       = "Fraction condition",
                    placeholder = "e.g. x == 5  or  x > 30",
                    width       = "360px")
        } else {
          selectInput(paste0("na_rm_", slot), label = "na.rm",
                      choices  = c("TRUE", "FALSE"),
                      selected = na_rm_val, width = "180px")
        }
      })
    })
  })
  
  # Extract button
  output$extract_btn_ui <- renderUI({
    v         <- vect_data()
    confirmed <- matchlyr_list()
    vec_attrs <- if (!is.null(input$col_matchvars)) { length(input$col_matchvars) } else { 0 }
    n_ext     <- length(confirmed)
    if (is.null(v) || (vec_attrs == 0 && n_ext == 0)) return(NULL)
    suffix_vec  <- if (vec_attrs > 1) { "s" } else { "" }
    suffix_ext  <- if (n_ext > 1) { "s" } else { "" }
    lbl_vec     <- if (vec_attrs > 0) { paste0(vec_attrs, " vector attribute", suffix_vec) } else { NULL }
    lbl_ext     <- if (n_ext > 0) { paste0(n_ext, " external layer", suffix_ext) } else { NULL }
    summary_str <- paste(c(lbl_vec, lbl_ext), collapse = " + ")
    div(style = "margin-top:8px; padding:20px 0 8px 0;",
        div(class = "section-divider"),
        div(style = "max-width:520px; margin-bottom:16px;",
            textInput("output_filename",
                      label       = "Output filename (without extension)",
                      placeholder = "e.g. my_study_covariates",
                      width       = "100%")
        ),
        div(style = "display:flex; align-items:center; gap:20px;",
            actionButton("run_extraction", label = "Extract covariate data",
                         class = "btn btn-success btn-lg"),
            p(style = "font-size:13px; color:#666; margin:0;",
              paste0("Ready to extract: ", summary_str, "."))
        ),
        uiOutput("extraction_result_ui")
    )
  })
  
  # Reactive val to hold extraction result and errors
  extraction_result <- reactiveVal(NULL)
  
  observeEvent(input$run_extraction, {
    v <- vect_data()
    if (is.null(v)) {
      showNotification("No vector file loaded. Please load a vector file first.",
                       type = "error")
      return()
    }
    
    fname <- trimws(input$output_filename)
    if (nchar(fname) == 0) {
      showNotification("Please enter an output filename before extracting.",
                       type = "warning")
      return()
    }
    
    # -- Build base_attrs: treatment col + optional UID + card-1 attributes ---
    uid_attr   <- if (isTRUE(input$use_uid)) { input$uid_field } else { NULL }
    base_attrs <- unique(c(input$ci_field, uid_attr, input$col_matchvars))
    
    # -- Build sources list from matchlyr_list --------------------------------
    # fun_custom, fun_fraction and na_rm are read from live input[[]] here
    # (they are intentionally not stored in matchlyr_list to avoid re-renders)
    ml      <- matchlyr_list()
    sources <- lapply(seq_along(ml), function(i) {
      src     <- ml[[i]]
      na_rm   <- !identical(input[[paste0("na_rm_", i)]], "FALSE")
      
      if (src$source_type %in% c("Local raster", "URL raster", "PEOPLE-ECCO dataset")) {
        list(
          type         = "raster",
          path         = src$path,
          url          = (src$source_type == "URL raster"),
          sel_lyr      = src$sel_lyr,
          fun          = src$fun,
          method       = src$resamp,
          fun_fraction = input[[paste0("fun_fraction_", i)]],
          fun_custom   = input[[paste0("fun_custom_",   i)]],
          na.rm        = na_rm,
          use_chunks   = isTRUE(src$use_chunks),
          chunk_size   = src$chunk_size,
          layer_prefix = tools::file_path_sans_ext(src$name)
        )
      } else if (src$source_type == "Local vector") {
        list(
          type         = "vector",
          path         = src$path,
          fun          = src$vec_fun,
          attrs        = src$vec_attrs,
          na.rm        = TRUE,
          layer_prefix = tools::file_path_sans_ext(src$name)
        )
      } else {
        NULL
      }
    })
    # Drop NULLs (unrecognised source types)
    sources <- Filter(Negate(is.null), sources)
    
    # -- Progress callback (wraps Shiny Progress object) ----------------------
    progress <- shiny::Progress$new()
    progress$set(message = "Starting extraction...", value = 0)
    on.exit(progress$close())
    
    shiny_progress <- function(msg, frac) {
      progress$set(value = frac, message = msg)
    }
    
    # -- Run ------------------------------------------------------------------
    result <- tryCatch(
      run_extraction(
        y            = v,
        base_attrs   = base_attrs,
        sources      = sources,
        progress_fun = shiny_progress
      ),
      error = function(e) {
        list(result = NULL,
             errors = paste0("Fatal error: ", conditionMessage(e)))
      }
    )
    
    if (!is.null(result$result)) {
      # Save to disk as GeoPackage
      out_path <- paste0(fname, ".gpkg")
      tryCatch(
        terra::writeVector(result$result, out_path, overwrite = TRUE),
        error = function(e) {
          result$errors <- c(result$errors,
                             paste0("Could not write file: ", conditionMessage(e)))
        }
      )
    }
    
    extraction_result(result)
  })
  
  output$extraction_result_ui <- renderUI({
    res <- extraction_result()
    if (is.null(res)) return(NULL)
    
    fname <- trimws(input$output_filename)
    
    if (!is.null(res$result)) {
      n_feat <- nrow(res$result)
      n_cols <- ncol(as.data.frame(res$result))
      ok_msg <- div(
        style = "margin-top:14px;",
        p(style = "color:#1e8449; font-weight:600; font-size:13px;",
          paste0("v Extraction complete: ", n_feat, " features, ",
                 n_cols, " attributes.")),
        p(style = "font-size:12px; color:#555;",
          paste0("Saved to: ", fname, ".gpkg"))
      )
    } else {
      ok_msg <- NULL
    }
    
    err_msgs <- if (length(res$errors) > 0) {
      div(style = "margin-top:8px;",
          lapply(res$errors, function(e) {
            p(style = "color:#c0392b; font-size:12px; margin:2px 0;",
              paste0("! ", e))
          })
      )
    } else {
      NULL
    }
    
    tagList(ok_msg, err_msgs)
  })
  
  # -- Tab 4: Matching input -------------------------------------------------
  
  # Reactive: resolve the matching SpatVector from either Tab 3 output
  # or a user-supplied file path.
  match_vect_data <- reactive({
    if (input$match_input_source == "from_tab") {
      res <- extraction_result()
      if (is.null(res) || is.null(res$result)) return(NULL)
      res$result
    } else {
      if (is.null(input$match_vect_file)) return(NULL)
      match_vect_from_file()
    }
  })
  
  # Reactive for the file-upload path (mirrors Tab 2 pattern)
  match_vect_from_file <- reactive({
    req(input$match_vect_file)
    paths    <- input$match_vect_file$datapath
    names_up <- input$match_vect_file$name
    new_paths <- file.path(dirname(paths), names_up)
    file.rename(paths, new_paths)
    is_shp <- endsWith(tolower(names_up), ".shp")
    entry  <- if (any(is_shp)) { new_paths[is_shp] } else { new_paths[1] }
    tryCatch(terra::vect(entry), error = function(e) {NULL})
  })
  
  # Status / metadata UI
  output$match_vect_status_ui <- renderUI({
    if (input$match_input_source == "from_tab") {
      res <- extraction_result()
      if (is.null(res) || is.null(res$result)) {
        return(p(class = "placeholder-msg",
                 "No extraction result yet. Run extraction in the Matching
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
                   height = paste0(max(280, length(res$vars_used) * 40), "px")),
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
  }, res = 96, bg = "white")
  
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
  # std.caliper is in a separate output so entering a caliper value does
  # not reset the caliper input itself.
  output$mi_caliper_ui <- renderUI({
    if (!input$mi_method %in% c("nearest","optimal","genetic","full","quick")) {
      return(NULL)
    }
    tagList(
      numericInput("mi_caliper",
                   label = "caliper (leave empty for no caliper)",
                   value = NA, min = 0, width = "100%"),
      uiOutput("mi_std_caliper_ui")
    )
  })
  
  output$mi_std_caliper_ui <- renderUI({
    cal <- input$mi_caliper
    if (is.null(cal) || is.na(cal)) return(NULL)
    selectInput("mi_std_caliper",
                label    = "std.caliper (caliper in SD units?)",
                choices  = c("TRUE","FALSE"), selected = "TRUE", width = "100%")
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
    
    if (is.null(v)) {
      showNotification("No matching dataset loaded.", type = "error")
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
      mi_args$distance <- input$mi_distance
      lnk <- input$mi_link
      if (!is.null(lnk) && nchar(lnk) > 0) mi_args$link <- lnk
    }
    
    # replace + ratio
    rep_val <- input$mi_replace
    if (!is.null(rep_val)) mi_args$replace <- as.logical(rep_val)
    rat_val <- input$mi_ratio
    if (!is.null(rat_val) && !is.na(rat_val)) mi_args$ratio <- as.integer(rat_val)
    
    # caliper
    cal_val <- input$mi_caliper
    if (!is.null(cal_val) && !is.na(cal_val)) {
      mi_args$caliper <- cal_val
      std_cal <- input$mi_std_caliper
      if (!is.null(std_cal)) mi_args$std.caliper <- as.logical(std_cal)
    }
    
    # m.order
    mord <- input$mi_m_order
    if (!is.null(mord) && nchar(mord) > 0) mi_args$m.order <- mord
    
    # discard + reestimate
    disc <- input$mi_discard
    if (!is.null(disc) && disc != "none") {
      mi_args$discard <- disc
      reest <- input$mi_reestimate
      if (!is.null(reest)) mi_args$reestimate <- as.logical(reest)
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
      if (!is.null(k2k_val)) mi_args$k2k <- as.logical(k2k_val)
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
    sf_obj    <- sf::st_as_sf(v)
    uid_field <- isolate(input$match_uid_field)
    if (!is.null(uid_field) && nchar(uid_field) > 0 && uid_field %in% names(sf_obj)) {
      sf_obj$.feature_id <- as.character(sf_obj[[uid_field]])
    } else {
      # Fallback: sequential (should not happen if UID was set correctly)
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
                   "Run matching in the Matching input tab to see results here.")
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
    
    v         <- res$matched_data
    treat_col <- isolate(input$match_treatment)
    n_treat   <- if (!is.null(treat_col) && treat_col %in% names(v)) {
      sum(as.data.frame(v)[[treat_col]] == 1, na.rm = TRUE)
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
    sf_obj    <- matched_sf()
    treat_col <- isolate(input$match_treatment)
    df        <- sf::st_drop_geometry(sf_obj)
    if (!is.null(treat_col) && treat_col %in% names(df)) {
      ifelse(df[[treat_col]] == 1, COL_TREAT, COL_CONTROL)
    } else {
      rep("#3498db", nrow(df))
    }
  })
  
  # Initial map render
  output$match_eval_leaflet <- renderLeaflet({
    sf_obj <- matched_sf()
    cols   <- feature_colours()
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
    drop_cols <- c(".feature_id", ".match_ids", ".weights", ".subclass")
    sub <- sub[, setdiff(names(sub), drop_cols), drop = FALSE]
    
    # Add a role column for clarity
    if (!is.null(treat_col) && treat_col %in% names(sub)) {
      sub$.role <- ifelse(sub[[treat_col]] == 1, "Treated", "Control")
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
  # -- Tab 6: Impact evaluation: matched pairs input ------------------------
  # =========================================================================
  
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
                 "No matching output yet. Run matching in the Matching
                  evaluation tab first."))
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
  
}  # end server