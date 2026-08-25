# =============================================================================
# tab4_impact_ui.R
# UI definition for Tab 4: Impact evaluation
# Sourced by ui.R
# =============================================================================

tab4_ui <- # -- Tab 6: Impact evaluation ---------------------------------------------
    tabPanel(
      title = "Impact evaluation",

      h3("Impact evaluation"),
      # p("Some info about this tab."),

      fluidRow(
        column(5,
          div(class = "card",
            div(class = "card-title",
              span(class = "icon", "\U0001f4c2"),
              "Matched control-impact pairs"
            ),
            p(style = "font-size:13px; color:#666; margin-bottom:16px;",
              "Provide the vector of matched control-impact pairs. Use
               the output from the Matching analysis tab, or load an
               existing file from disk."),
            radioButtons("baci_input_source",
              label    = NULL,
              choices  = c(
                "Use output from Matching evaluation tab" = "from_matching",
                "Load from file"                          = "from_file"
              ),
              selected = "from_matching"
            ),
            conditionalPanel(
              condition = "input.baci_input_source == 'from_file'",
              div(style = "margin-top:8px;",
                fileInput("baci_vect_file",
                  label       = "Browse for a vector file",
                  accept      = c(".gpkg", ".geojson", ".json", ".shp",
                                  ".dbf", ".shx", ".prj", ".cpg"),
                  multiple    = TRUE,
                  buttonLabel = "Browse\u2026",
                  placeholder = "No file selected")
              )
            ),
            uiOutput("baci_vect_status_ui"),

            div(class = "section-divider"),

            # Column role selectors (pre-filled from matching tab when available)
            uiOutput("baci_col_selectors_ui")
          )
        ),
        column(7,
          uiOutput("baci_plot_card_ui")
        )
      ),

      # Card 2: impact assessment method selection
      uiOutput("baci_method_card_ui"),

      # Static map controls: always in DOM, populated via updateSelectInput
      # when results arrive. Must NOT be inside renderUI to stay stable.
      conditionalPanel(
        condition = "output.baci_results_ready",
        div(class = "card",
          div(class = "card-title",
            span(class = "icon", "\U0001f5fa"),
            "Impact assessment results"),
          uiOutput("baci_results_summary_ui"),
          fluidRow(
            column(6,
              selectInput("baci_map_var",
                label    = "Variable to visualise",
                choices  = character(0),
                width    = "100%")
            ),
            column(6,
              div(style = "margin-top:25px;",
                checkboxInput("baci_grey_nonsig",
                  label = "Grey out non-significant units (p > 0.05)",
                  value = FALSE)
              )
            )
          ),
          leafletOutput("baci_result_map", height = "460px"),
          uiOutput("baci_result_detail_ui")
        ),
        # Pooled results table (shown instead of map for pooled analysis)
        uiOutput("baci_pooled_card_ui"),

        # Save card: below results
        uiOutput("baci_save_card_ui")
      )
)
