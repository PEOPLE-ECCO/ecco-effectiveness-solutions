# ============================================================
#  Shared Shiny module: vector preview card
#  Defines vector_plot_ui() and vector_plot_server()
#  Usage:
#    UI:     vector_plot_ui("my_ns_id")
#    Server: vector_plot_server("my_ns_id", vect_reactive)
#            where vect_reactive is a reactive() returning a SpatVector or NULL
# ============================================================

# Null-coalescing helper (may also be defined in server.R; safe to re-define)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}

# Common colour palettes available for terra::plot()
.vp_palettes <- c(
  "Default (terra)"  = "default",
  "Viridis"          = "viridis",
  "Magma"            = "magma",
  "Plasma"           = "plasma",
  "Inferno"          = "inferno",
  "Terrain"          = "terrain",
  "Topographic"      = "topo",
  "Heat"             = "heat",
  "Grays"            = "grays",
  "Red-Yellow-Green" = "RdYlGn",
  "Red-Yellow-Blue"  = "RdYlBu",
  "Spectral"         = "Spectral",
  "Blue-Red"         = "BrBG"
)

# Resolve a palette name to a colour vector (n colours)
.vp_palette_cols <- function(pal_name, n = 100) {
  switch(pal_name,
    "viridis"  = viridis::viridis(n),
    "magma"    = viridis::magma(n),
    "plasma"   = viridis::plasma(n),
    "inferno"  = viridis::inferno(n),
    "terrain"  = terrain.colors(n),
    "topo"     = topo.colors(n),
    "heat"     = heat.colors(n),
    "grays"    = grDevices::gray.colors(n),
    "RdYlGn"   = grDevices::colorRampPalette(
                   RColorBrewer::brewer.pal(11, "RdYlGn"))(n),
    "RdYlBu"   = grDevices::colorRampPalette(
                   RColorBrewer::brewer.pal(11, "RdYlBu"))(n),
    "Spectral"  = grDevices::colorRampPalette(
                   RColorBrewer::brewer.pal(11, "Spectral"))(n),
    "BrBG"      = grDevices::colorRampPalette(
                   RColorBrewer::brewer.pal(11, "BrBG"))(n),
    NULL  # NULL = terra default
  )
}

# ------------------------------------------------------------------
# UI function
# ------------------------------------------------------------------
vector_plot_ui <- function(id, card_title = "\U0001f5fa️ Vector Preview") {
  ns <- NS(id)
  div(class = "card", style = "height:100%;",
    div(class = "card-title", HTML(card_title)),

    # Attribute selector (populated server-side)
    uiOutput(ns("attr_selector_ui")),

    # Extra customisation controls (shown below attribute selector)
    uiOutput(ns("plot_options_ui")),

    # Plot (CRS label is drawn inside the plot margin by the server)
    plotOutput(ns("plot"), height = "460px")
  )
}

# ------------------------------------------------------------------
# Server function
# ------------------------------------------------------------------
# vect_reactive : a reactive() that returns a SpatVector or NULL
vector_plot_server <- function(id, vect_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Attribute selector -------------------------------------------
    output$attr_selector_ui <- renderUI({
      v <- vect_reactive()
      if (is.null(v) || !inherits(v, "SpatVector")) return(NULL)
      attr_names <- names(v)
      if (length(attr_names) == 0) return(NULL)
      selectInput(ns("plot_attr"),
        label    = "Attribute to visualise",
        choices  = setNames(attr_names, attr_names),
        selected = attr_names[1],
        width    = "100%")
    })

    # --- Helpers ------------------------------------------------------

    # Cache datatypes and numeric flags once when the vector loads
    all_meta <- reactive({
      v <- vect_reactive()
      if (is.null(v) || !inherits(v, "SpatVector")) return(NULL)
      dt           <- terra::datatype(v)
      names(dt)    <- names(v)
      is_num       <- tolower(dt) != "string"
      list(datatypes = dt, is_num = is_num)
    })

    # Cache all ranges at once using range(v) — character cols return NA
    all_ranges <- reactive({
      v <- vect_reactive()
      if (is.null(v) || !inherits(v, "SpatVector")) return(NULL)
      tryCatch(range(v, na.rm = TRUE), error = function(e) NULL)
    })

    # Sensible slider step from range span
    .slider_step <- function(rng) {
      span <- rng[2] - rng[1]
      if (span == 0)   return(1)
      if (span > 100)  return(signif(span / 100, 1))
      if (span > 1)    return(signif(span / 1000, 1))
      signif(span / 1000, 1)
    }

    # --- Current attribute (reactive) --------------------------------
    cur_attr <- reactive({
      v <- vect_reactive()
      if (is.null(v) || !inherits(v, "SpatVector")) return(NULL)
      attr <- input$plot_attr
      nms  <- names(v)
      if (!is.null(attr) && attr %in% nms) attr else if (length(nms) > 0) nms[1] else NULL
    })

    # --- Plot options: re-render when attribute changes ----------------------
    # We re-render the whole options block on attribute change so that the
    # slider min/max/value are always correct. Plot type and palette are
    # preserved across re-renders via the `selected` argument reading the
    # *current* input value (isolate so we don't create a dependency on them).
    output$plot_options_ui <- renderUI({
      v <- vect_reactive()
      if (is.null(v) || !inherits(v, "SpatVector")) return(NULL)

      # cur_attr() depends on input$plot_attr, which is what we want here:
      # re-render when the user picks a different attribute.
      attr <- cur_attr()

      # Read first attribute directly from vector if input not yet available
      if (is.null(attr)) {
        nms  <- names(v)
        attr <- if (length(nms) > 0) nms[1] else NULL
      }

      is_num <- isTRUE(all_meta()$is_num[attr])
      rng    <- tryCatch(as.numeric(all_ranges()[, attr]),
                         error = function(e) c(0, 1))
      if (any(!is.finite(rng))) rng <- c(0, 1)
      step   <- .slider_step(rng)

      # Preserve user's current palette selection across re-renders
      cur_col  <- isolate(input$plot_col)  %||% "default"
      cur_main <- isolate(input$plot_main) %||% (attr %||% "")

      tagList(
        # Plot type: only shown for numeric attributes (categorical is the only
        # sensible choice for character/factor, so no selector needed there)
        if (is_num) {
          # Preserve user's current type selection
          cur_type <- isolate(input$plot_type) %||% "continuous"
          tagList(
            selectInput(ns("plot_type"),
              label    = "Plot type",
              choices  = c("Continuous" = "continuous", "Categorical" = "classes"),
              selected = cur_type,
              width    = "100%"),
            # Range slider — only for continuous
            conditionalPanel(
              condition = sprintf("input['%s'] === 'continuous'", ns("plot_type")),
              sliderInput(ns("plot_range"),
                label = "Value range (values outside range shown in border colour)",
                min   = rng[1], max = rng[2],
                value = c(rng[1], rng[2]),
                step  = step, width = "100%")
            )
          )
        },
        # Colour palette
        selectInput(ns("plot_col"),
          label    = "Colour palette",
          choices  = .vp_palettes,
          selected = cur_col,
          width    = "100%"),
        # Title — pre-filled with attribute name; updated when attr changes
        textInput(ns("plot_main"),
          label       = "Plot title",
          value       = attr %||% "",
          placeholder = "Leave blank for no title",
          width       = "100%")
      )
    })


    # --- CRS label (rendered into the plot, not a separate UI element) -------
    crs_label <- reactive({
      v <- vect_reactive()
      if (is.null(v) || !inherits(v, "SpatVector")) return("")
      crs_str <- tryCatch(terra::crs(v, describe = TRUE), error = function(e) NULL)
      if (!is.null(crs_str) && is.data.frame(crs_str)) {
        name <- crs_str$name[1]
        auth <- crs_str$authority[1]
        code <- crs_str$code[1]
        if (!is.na(name)) {
          paste0("CRS: ", name,
                 if (!is.na(code)) paste0(" (", auth, ":", code, ")") else "")
        } else {
          paste0("CRS: ", tryCatch(terra::crs(v), error = function(e) "Unknown"))
        }
      } else {
        crs_raw <- tryCatch(terra::crs(v), error = function(e) "")
        if (nchar(crs_raw) == 0) "CRS: Not defined" else paste0("CRS: ", crs_raw)
      }
    })

    # --- Plot ---------------------------------------------------------
    output$plot <- renderPlot({
      v <- vect_reactive()
      req(!is.null(v), inherits(v, "SpatVector"))

      attr      <- cur_attr()
      is_num    <- isTRUE(all_meta()$is_num[attr])
      plot_type <- if (is_num) (input$plot_type %||% "continuous") else "classes"
      pal_name  <- input$plot_col  %||% "default"
      main_txt  <- input$plot_main  # blank string = no title
      rng_vals  <- input$plot_range  # NULL until slider renders
      crs_lbl   <- crs_label()

      # Title: use as-is (blank = no title passed to terra::plot)
      ttl <- if (!is.null(main_txt)) trimws(main_txt) else ""

      # Colour vector (NULL = terra default)
      cols <- if (pal_name == "default") NULL else .vp_palette_cols(pal_name)

      # Build terra::plot() call
      plot_args <- list(
        x          = v,
        y          = attr,
        main       = ttl,
        type       = plot_type,
        fill_range = TRUE
      )
      if (!is.null(cols)) plot_args$col <- cols
      # Apply range slider for continuous plots
      if (plot_type == "continuous" && !is.null(rng_vals) && length(rng_vals) == 2) {
        plot_args$range <- rng_vals
      }

      # Add bottom outer margin for CRS label (mar is inner; oma is outer)
      old_par <- par(oma = c(1.5, 0, 0, 0))
      on.exit(par(old_par), add = TRUE)

      tryCatch(
        do.call(terra::plot, plot_args),
        error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Plot error:", conditionMessage(e)),
               cex = 0.85, col = "red")
        }
      )

      # Draw CRS label in outer bottom-right margin
      if (nchar(crs_lbl) > 0) {
        mtext(crs_lbl, side = 1, line = 0.3, adj = 1,
              outer = TRUE, cex = 0.72, col = "#666666")
      }
    }, res = 96, bg = "white")
  })
}
