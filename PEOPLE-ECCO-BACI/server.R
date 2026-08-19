server <- function(input, output, session) {
  
  # Null coalescing helper
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # Helper: convert SpatVector to sf in WGS84 for leaflet display.
  # Leaflet requires EPSG:4326; this reprojects for display only -- the
  # original SpatVector is never modified.
  to_leaflet_sf <- function(v) {
    sf_obj <- sf::st_as_sf(v)
    if (!sf::st_is_longlat(sf_obj)) {
      sf_obj <- sf::st_transform(sf_obj, 4326)
    }
    sf_obj
  }
  
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
    attr_names <- names(v)
    if (length(attr_names) == 0) return(NULL)
    selectInput("ci_field", label = "Treatment attribute",
                choices  = setNames(attr_names, attr_names),
                selected = attr_names[1], width = "100%")
  })
  
  # -- Vector: unique ID card ---------------------------------------------------
  
  output$attr_card_ui <- renderUI({
    v <- vect_data()
    if (is.null(v)) return(NULL)
    attr_names <- names(v)
    if (length(attr_names) == 0) return(NULL)
    tagList(
      div(style = "display:flex; align-items:center; gap:10px;",
          checkboxInput(inputId = "use_uid", label = NULL, value = FALSE),
          p(style = "font-size:13px; color:#666; margin:0;",
            "Use a unique feature ID attribute")
      ),
      conditionalPanel(
        condition = "input.use_uid == true",
        selectInput("uid_field", label = "Unique ID attribute",
                    choices  = setNames(attr_names, attr_names),
                    selected = NULL, width = "100%"),
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
    attr_names <- names(v)
    div(class = "card", style = "height:100%;",
        div(class = "card-title",
            span(class = "icon", "\U0001f5fa"), "Vector Preview"),
        if (length(attr_names) > 0) {
          selectInput("vec_plot_attr",
                      label    = "Attribute to visualise",
                      choices  = setNames(attr_names, attr_names),
                      selected = attr_names[1],
                      width    = "100%")
        } else { NULL },
        plotOutput("vector_plot", height = "480px")
    )
  })
  
  output$vector_plot <- renderPlot({
    v <- vect_data()
    req(!is.null(v))
    attr <- input$vec_plot_attr
    if (!is.null(attr) && nchar(attr) > 0 && attr %in% names(v)) {
      terra::plot(v, attr, main = attr)
    } else if (length(names(v)) > 0) {
      terra::plot(v, names(v)[1], main = names(v)[1])
    } else {
      terra::plot(v)
    }
  }, res = 96, bg = "white")
  
  output$retain_cols_ui <- renderUI({
    v <- vect_data()
    if (is.null(v)) return(NULL)
    attr_names <- names(v)
    div(style = "margin-top:8px;",
        selectInput("retain_cols",
                    label    = "Attributes to retain in output (default: all)",
                    choices  = setNames(attr_names, attr_names),
                    selected = attr_names,
                    multiple = TRUE,
                    width    = "100%")
    )
  })
  
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
      } else if (r$source_type == "openEO") {
        div(class = "info-row", style = "margin-left:34px; margin-top:4px;",
            span(class = "info-pill", "openEO"),
            span(class = "info-pill", r$collection),
            span(class = "info-pill", "EPSG:4326")
        )
      } else if (r$source_type == "openEO") {
        div(class = "info-row", style = "margin-left:34px; margin-top:4px;",
            span(class = "info-pill", "openEO"),
            span(class = "info-pill", r$collection),
            span(class = "info-pill", "EPSG:4326")
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
              } else if (r$source_type == "openEO") {
                tagList(
                  p(style = "font-size:12px; color:#555; margin-bottom:4px;",
                    paste0("Collection: ", r$collection)),
                  selectInput(paste0("oe_bands_", i),
                              label    = "Band(s)",
                              choices  = r$lyr_choices,
                              selected = r$sel_lyr,
                              multiple = TRUE, width = "100%"),
                  selectInput(paste0("oe_reducer_", i),
                              label    = "Spatial reducer",
                              choices  = c("mean","median","min","max","sum","sd"),
                              selected = r$fun, width = "100%")
                )
              } else if (r$source_type == "openEO") {
                tagList(
                  p(style = "font-size:12px; color:#555; margin-bottom:4px;",
                    paste0("Collection: ", r$collection)),
                  selectInput(paste0("oe_bands_", i),
                              label    = "Band(s)", choices = r$lyr_choices,
                              selected = r$sel_lyr, multiple = TRUE, width = "100%"),
                  selectInput(paste0("oe_reducer_", i),
                              label    = "Spatial reducer",
                              choices  = c("mean","median","min","max","sum","sd"),
                              selected = r$fun, width = "100%")
                )
              } else {
                # Local vector controls
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
                                           choices  = c("Select..." = "", "Local raster" = "Local raster", "URL raster" = "URL raster", "PEOPLE-ECCO dataset" = "PEOPLE-ECCO dataset", "Local vector" = "Local vector", "openEO collection" = "openEO"),
                                           selected = src_selected, width = "100%")
                           )
    )
    
    is_local_raster <- (!is.null(src_type_val) && src_type_val == "Local raster")
    is_url_raster   <- (!is.null(src_type_val) && src_type_val == "URL raster")
    is_local_vector <- (!is.null(src_type_val) && src_type_val == "Local vector")
    is_openeo       <- (!is.null(src_type_val) && src_type_val == "openEO")
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
    } else if (isTRUE(is_openeo)) {
      div(style = "margin-left:34px; margin-top:8px;",
          div(style = "display:flex; justify-content:flex-end;",
              actionButton(next_btn_id, "Add collection",
                           class = "btn btn-primary btn-sm")
          ),
          uiOutput(paste0("lyrstatus_", next_index))
      )
    } else {
      NULL
    }
    
    # Add openEO collection + reducer selectors when openEO source type selected
    openeo_pending <- if (isTRUE(is_openeo)) {
      div(style = "margin-top:8px;",
          selectInput(paste0("oe_collection_", next_index),
                      label    = "Collection",
                      choices  = openeo_collection_choices(),
                      selected = names(openeo_collection_choices())[1],
                      width    = "100%"),
          selectInput(paste0("oe_reducer_", next_index),
                      label    = "Spatial reducer",
                      choices  = c("mean","median","min","max","sum","sd"),
                      selected = "mean", width = "100%")
      )
    } else { NULL }
    
    do.call(tagList, c(confirmed_rows, list(
      div(style = "margin-top:4px;", source_selector, path_controls, openeo_pending)
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
        } else if (!is.null(src) && src == "openEO") {
          coll_sel <- input[[paste0("oe_collection_", slot)]]
          if (is.null(coll_sel) || nchar(coll_sel) == 0) {
            list(ok = FALSE, msg = "Please select a collection first.")
          } else {
            list(ok = TRUE, msg = "",
                 source_type = "openEO",
                 name        = coll_sel,
                 path        = "",
                 collection  = coll_sel,
                 crs         = "EPSG:4326",
                 nlyr        = 1L, nrow = NA_integer_, ncol = NA_integer_,
                 res         = c(NA_real_, NA_real_),
                 lyr_choices = openeo_band_choices(coll_sel),
                 sel_lyr     = openeo_band_choices(coll_sel),
                 fun         = "mean", resamp = "bilinear",
                 na_rm       = "TRUE", use_chunks = FALSE,
                 chunk_size  = 500L)
          }
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
    if (is.null(v)) return(NULL)
    confirmed <- matchlyr_list()
    n_ext     <- length(confirmed)
    summary_str <- if (n_ext > 0) {
      paste0("Input vector + ", n_ext, " external source(s)")
    } else {
      "Input vector attributes only"
    }
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
    
    # -- Build base_attrs from retain_cols (all by default) ------------------
    retain     <- input$retain_cols
    base_attrs <- if (!is.null(retain) && length(retain) > 0) { retain } else { names(v) }
    
    # -- Build sources list from matchlyr_list --------------------------------
    # fun_custom, fun_fraction and na_rm are read from live input[[]] here
    # (they are intentionally not stored in matchlyr_list to avoid re-renders)
    ml      <- matchlyr_list()
    sources <- lapply(seq_along(ml), function(i) {
      src     <- ml[[i]]
      na_rm   <- !identical(input[[paste0("na_rm_", i)]], "FALSE")
      
      if (src$source_type == "openEO") {
        list(
          type         = "openeo",
          collection   = src$collection,
          bands        = input[[paste0("oe_bands_", i)]],
          reducer      = {
            rv <- input[[paste0("oe_reducer_", i)]]
            if (!is.null(rv) && nchar(rv) > 0) { rv } else { "mean" }
          },
          layer_prefix = make_colname(src$collection)
        )
      } else if (src$source_type == "openEO") {
        list(
          type         = "openeo",
          collection   = src$collection,
          bands        = input[[paste0("oe_bands_", i)]],
          reducer      = {
            rv <- input[[paste0("oe_reducer_", i)]]
            if (!is.null(rv) && nchar(rv) > 0) { rv } else { "mean" }
          },
          layer_prefix = make_colname(src$collection)
        )
      } else if (src$source_type %in% c("Local raster", "URL raster", "PEOPLE-ECCO dataset")) {
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
        col_uid      = if (isTRUE(input$use_uid) && nchar(input$uid_field) > 0) {
          input$uid_field
        } else { NULL },
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
    sf_obj    <- matched_sf()
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
                              "PEOPLE-ECCO raster"     = "PEOPLE-ECCO dataset"
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
      is_raster <- src_val %in% c("Local raster", "URL raster", "PEOPLE-ECCO dataset")
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
      selectInput(paste0("ie_vecfun_", slot_id),
                  label    = "Function",
                  choices  = c("mean","median","sd","min","max","sum","count","minDistance"),
                  selected = r$vec_fun, width = "100%")
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
        } else if (!is.null(src) && src == "PEOPLE-ECCO dataset") {
          validate_peopleecco(path)
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
  # Reads impact_result() directly (not via baci_result_sf_val()) to avoid
  # shared reactive chains that cause spurious invalidation of the selector.
  baci_map_colours <- reactive({
    res    <- impact_result()
    req(!is.null(res) && !is.null(res$result_vect))
    df     <- as.data.frame(res$result_vect)
    contrast_col <- if (!is.null(input$baci_map_var) && nchar(input$baci_map_var) > 0) {
      input$baci_map_var
    } else {
      paste0(res$effect_cols[1], "_contrast")
    }
    if (!contrast_col %in% names(df)) return(NULL)
    
    vals <- df[[contrast_col]]
    rng  <- range(vals, na.rm = TRUE)
    # Symmetric range for diverging palette
    abs_max <- max(abs(rng), na.rm = TRUE)
    pal <- leaflet::colorNumeric(
      palette = c("#2980b9","white","#c0392b"),
      domain  = c(-abs_max, abs_max), na.color = "#aaaaaa")
    cols <- pal(vals)
    
    # Significance greying: find matching p-value column
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
      layerId  = "baci_legend",
      position = "bottomright",
      pal      = cm$pal, values = cm$vals,
      title    = cm$contrast_col, opacity = 0.8)
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
        layerId  = "baci_legend",
        position = "bottomright",
        pal      = cm$pal,
        values   = cm$vals,
        title    = cm$contrast_col,
        opacity  = 0.8)
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
    result_rows <- lapply(effect_cols, function(ec) {
      data.frame(
        Variable = ec,
        Contrast = if (paste0(ec,"_contrast") %in% names(row)) {
          round(row[[paste0(ec,"_contrast")]], 4)
        } else { NA_real_ },
        P.value  = if (paste0(ec,"_pvalue") %in% names(row)) {
          pv <- row[[paste0(ec,"_pvalue")]]
          if (is.na(pv)) NA_real_ else round(pv, 4)
        } else { NA_real_ },
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, result_rows)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "NA")
  
}  # end server