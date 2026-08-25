# =============================================================================
# tab2_covariates_server.R
# Server logic for Tab 2: Extract matching covariates
# Sourced inside server() with local=TRUE - has access to input/output/session

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
                    {
                      # count: only meaningful when units=polygons, covariate=points
                      units_geom <- geom_info()$types   # from vect_data reactive
                      cov_geom   <- r$geom_type
                      show_count <- any(grepl("polygon", tolower(units_geom))) &&
                                    grepl("point", tolower(cov_geom), ignore.case = TRUE)
                      fun_choices <- c("mean","median","sd","min","max","sum",
                                         "minDistance","merge")
                      if (show_count) { fun_choices <- c(fun_choices, "count") }
                      selectInput(paste0("vec_fun_", i), label = "Function",
                        choices  = fun_choices,
                        selected = if (r$vec_fun %in% fun_choices) r$vec_fun else "mean",
                        width    = "100%")
                    }
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
                ),
                # Merge controls: shown when function == "merge"
                if (isTRUE(r$vec_fun == "merge")) {
                  tagList(
                    radioButtons(paste0("vec_merge_by_", i),
                      label    = "Merge by",
                      choices  = c("Matching geometry" = "geometry",
                                   "Attribute ID"      = "id"),
                      selected = if (!is.null(r$merge_by)) r$merge_by else "geometry",
                      inline   = TRUE),
                    conditionalPanel(
                      condition = paste0("input['vec_merge_by_", i, "'] == 'id'"),
                      selectInput(paste0("vec_merge_id_", i),
                        label    = "ID field in covariate vector",
                        choices  = setNames(r$attr_names, r$attr_names),
                        selected = if (!is.null(r$merge_id_field)) {
                          r$merge_id_field
                        } else { r$attr_names[1] },
                        width    = "100%")
                    ),
                    selectInput(paste0("vec_merge_attrs_", i),
                      label    = "Attributes to merge",
                      choices  = setNames(r$attr_names, r$attr_names),
                      selected = if (!is.null(r$merge_attrs)) {
                        r$merge_attrs
                      } else { r$attr_names },
                      multiple = TRUE,
                      width    = "100%")
                  )
                } else { NULL }
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
          choices  = c("Select..." = "", "Local raster" = "Local raster", "URL raster" = "URL raster", "Local vector" = "Local vector", "openEO collection" = "openEO"),
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

      observeEvent(input[[paste0("vec_merge_by_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]]$merge_by <- input[[paste0("vec_merge_by_", slot)]]
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      observeEvent(input[[paste0("vec_merge_id_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]]$merge_id_field <- input[[paste0("vec_merge_id_", slot)]]
          matchlyr_list(current)
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      observeEvent(input[[paste0("vec_merge_attrs_", slot)]], {
        current <- matchlyr_list()
        if (slot <= length(current)) {
          current[[slot]]$merge_attrs <- input[[paste0("vec_merge_attrs_", slot)]]
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
        if (isTRUE(src$vec_fun == "merge")) {
          list(
            type           = "vector_merge",
            path           = src$path,
            merge_by       = {
              mb <- input[[paste0("vec_merge_by_", i)]]
              if (!is.null(mb) && nchar(mb) > 0) { mb } else { "geometry" }
            },
            merge_id_field = input[[paste0("vec_merge_id_", i)]],
            merge_attrs    = input[[paste0("vec_merge_attrs_", i)]],
            layer_prefix   = tools::file_path_sans_ext(src$name)
          )
        } else {
          list(
            type         = "vector",
            path         = src$path,
            fun          = src$vec_fun,
            attrs        = src$vec_attrs,
            na.rm        = TRUE,
            layer_prefix = tools::file_path_sans_ext(src$name)
          )
        }
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

    shiny_progress <- function(msg, frac = NULL) {
      if (!is.null(frac)) {
        progress$set(value = frac, message = msg)
      } else {
        # Single-argument call from openEO notify() - show as popup notification
        progress$set(message = msg)
        showNotification(msg, type = "message", duration = 6)
      }
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
