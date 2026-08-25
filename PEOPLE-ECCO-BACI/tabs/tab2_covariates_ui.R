# =============================================================================
# tab2_covariates_ui.R
# UI definition for Tab 2: Extract matching covariates
# Sourced by ui.R
# =============================================================================

tab2_ui <- # -- Tab 2: Extract matching covariates ----------------------------------
    tabPanel(
      title = "Extract matching covariates",

      h3("Extract matching covariates"),

      # -- Section 1: Units of analysis + vector preview side by side --------
      fluidRow(
        column(5,
          div(class = "card",
            div(class = "card-title",
              span(class = "icon", "\U0001f4c2"),
              "Units of analysis"
            ),
            p(style = "font-size:13px; color:#666; margin-bottom:18px;",
              "Upload the vector file containing your units of analysis.
               Allowed formats: GeoJSON, GeoPackage, or Shapefile (select all associated
               files for Shapefile)."),
            fileInput(
              inputId     = "geojson_file",
              label       = "Browse for a vector file",
              accept      = c(".gpkg",".geojson",".json",".shp",".dbf",
                              ".shx",".prj",".cpg",".qpj"),
              multiple    = TRUE,
              buttonLabel = "Browse\u2026",
              placeholder = "No file selected"
            ),
            uiOutput("geom_type_ui"),
            # These appear once a file is loaded
            uiOutput("select_ci_card_ui"),
            uiOutput("attr_card_ui"),
            uiOutput("retain_cols_ui")
          )
        ),
        column(7,
          uiOutput("vector_plot_card_ui")
        )
      ),

      # -- Section 2: Additional covariate sources ---------------------------
      div(style = "margin-top:8px;",
        # h4("Additional covariate sources"),
        # p(style = "font-size:13px; color:#666;",
        #   "Optionally add covariate layers from external sources to extract
        #    and append to the output vector. If no sources are added, the
        #    output will contain only the input vector attributes.")
      ),

      div(class = "card",
        div(class = "card-title",
          span(class = "icon", "\U0001f4be"),
          "Add covariates"
        ),
        p(style = "font-size:13px; color:#666; margin-bottom:20px;",
          "Select the source type; options appear below. Each confirmed
           source is appended automatically."),
        uiOutput("matchlyr_rows_ui")
      ),

      uiOutput("extract_btn_ui")
    )
