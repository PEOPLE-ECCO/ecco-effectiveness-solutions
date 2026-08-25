# =============================================================================
# tab3_matching_ui.R
# UI definition for Tab 3: Matching analysis
# Sourced by ui.R
# =============================================================================

tab3_ui <- # -- Tab 3: Matching analysis --------------------------------------------
    tabPanel(
      title = "Matching analysis",

      h3("Matching analysis"),
      # p("Set up the matching dataset, configure parameters, run matching,
      #    and evaluate results."),

      # -- Section 1: Input & parameters ------------------------------------
      fluidRow(
        column(5,
          div(class = "card",
            div(class = "card-title",
              span(class = "icon", "\U0001f4c2"),
              "Matching dataset"
            ),
            p(style = "font-size:13px; color:#666; margin-bottom:16px;",
              "Provide the vector containing treatment indicator and
               matching covariates. Use the output from the 'Extract matching covariates'
               tab, or load an existing file from disk."),
            radioButtons("match_input_source",
              label    = NULL,
              choices  = c(
                "Use output from Extract matching covariates" = "from_tab",
                "Load from file"                              = "from_file"
              ),
              selected = "from_tab"
            ),
            conditionalPanel(
              condition = "input.match_input_source == 'from_file'",
              div(style = "margin-top:8px;",
                fileInput("match_vect_file",
                  label       = "Browse for a vector file",
                  accept      = c(".gpkg", ".geojson", ".json", ".shp",
                                  ".dbf", ".shx", ".prj", ".cpg"),
                  multiple    = TRUE,
                  buttonLabel = "Browse\u2026",
                  placeholder = "No file selected")
              )
            ),
            uiOutput("match_vect_status_ui"),
            div(class = "section-divider"),
            uiOutput("match_uid_ui"),
            uiOutput("match_treatment_ui"),
            uiOutput("match_covars_ui"),
            uiOutput("match_multicol_check_ui"),
            uiOutput("match_attr_select_ui")
          ),

          div(class = "card",
            div(class = "card-title",
              span(class = "icon", "\u2699"),
              "Matching parameters"
            ),
            p(style = "font-size:13px; color:#666; margin-bottom:16px;",
              "Parameters passed to ",
              tags$code("MatchIt::matchit()"),
              ". Formula and data are generated from the selections above."),
            selectInput("mi_method", label = "method",
              choices  = c("nearest","optimal","full","quick","genetic",
                           "cem","exact","cardinality","subclass"),
              selected = "nearest", width = "100%"),
            selectInput("mi_estimand", label = "estimand",
              choices  = c("ATT","ATC","ATE"),
              selected = "ATT", width = "100%"),
            uiOutput("mi_distance_ui"),
            uiOutput("mi_replace_ratio_ui"),
            uiOutput("mi_caliper_ui"),
            uiOutput("mi_morder_ui"),
            uiOutput("mi_discard_ui"),
            uiOutput("mi_exact_ui"),
            uiOutput("mi_method_extras_ui"),
            uiOutput("mi_sweights_ui"),
            uiOutput("mi_mahvars_ui"),
            uiOutput("mi_dist_options_ui"),
            div(style = "margin-top:20px;",
              actionButton("click_matching", "Run matching",
                class = "btn-success btn-lg", width = "100%")
            )
          )
        ),

        column(7,
          uiOutput("match_plot_card_ui"),
          uiOutput("match_multicol_output_ui")
        )
      ),

      # -- Section 2: Evaluation (appears after matching) -------------------
      div(style = "margin-top:8px;",
        h3("Matching evaluation"),
        # p(style = "font-size:13px; color:#666;",
        #   "Review matched units, assess balance, and save results.")
      ),

      uiOutput("match_eval_dropped_card_ui"),
      uiOutput("match_eval_map_card_ui"),
      uiOutput("match_eval_attr_card_ui"),
      uiOutput("match_eval_diag_card_ui"),
      uiOutput("match_eval_save_card_ui")
    )
