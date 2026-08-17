ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    body { background-color:#f4f6f9; font-family:'Segoe UI',Arial,sans-serif; color:#2c3e50; }
    .app-header { background:#00AF9E; color:white; padding:0 30px 0 0; margin-bottom:0; border-radius:0; display:flex; align-items:stretch; min-height:64px; }
    .app-header img { height:64px; width:auto; display:block; margin-right:20px; object-fit:cover; flex-shrink:0; }
    .app-header-text { display:flex; flex-direction:column; justify-content:center; }
    .app-header h2 { margin:0; font-size:22px; font-weight:700; letter-spacing:0.5px; }
    .app-header p  { margin:3px 0 0 0; font-size:12px; opacity:0.85; }
    .nav-tabs { background-color:#ffffff; border-bottom:2px solid #00AF9E; padding:0 20px; margin-bottom:0; }
    .nav-tabs > li > a { color:#555; border:none !important; border-radius:0 !important; padding:12px 20px; font-weight:500; font-size:14px; margin-right:4px; transition:color 0.2s; }
    .nav-tabs > li > a:hover { background:transparent; color:#00AF9E; border-bottom:2px solid #00AF9E !important; }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus { color:#00AF9E !important; border-bottom:3px solid #00AF9E !important; background:transparent; font-weight:600; }
    .tab-content { background:transparent; padding:28px 30px; }
    .card { background:#ffffff; border-radius:8px; box-shadow:0 1px 4px rgba(0,0,0,0.08); padding:24px 28px; margin-bottom:20px; }
    .card-title { font-size:15px; font-weight:600; color:#2c3e50; margin-bottom:16px; padding-bottom:10px; border-bottom:1px solid #eef0f3; display:flex; align-items:center; gap:8px; }
    .card-title .icon { width:26px; height:26px; border-radius:6px; background:#ebf5fb; display:inline-flex; align-items:center; justify-content:center; font-size:14px; }
    .form-group label { font-weight:500; font-size:13px; color:#555; margin-bottom:6px; }
    .geom-badge { display:inline-flex; align-items:center; gap:8px; padding:8px 16px; border-radius:20px; font-size:13px; font-weight:600; letter-spacing:0.3px; }
    .geom-polygon { background:#eafaf1; color:#1e8449; border:1px solid #a9dfbf; }
    .geom-lines   { background:#fef9e7; color:#b7770d; border:1px solid #f9e79f; }
    .geom-points  { background:#ebf5fb; color:#1a6fa0; border:1px solid #aed6f1; }
    .geom-error   { background:#fdedec; color:#c0392b; border:1px solid #f5b7b1; }
    .info-row { display:flex; align-items:center; gap:12px; font-size:13px; color:#666; margin-top:10px; }
    .info-pill { background:#f0f3f6; border-radius:12px; padding:4px 12px; font-size:12px; color:#555; }
    .selectize-control .selectize-input { border-radius:6px; border-color:#d5d8dc; font-size:13px; box-shadow:none; }
    .selectize-control .selectize-input.focus { border-color:#00AF9E; box-shadow:0 0 0 2px rgba(0,175,158,0.15); }
    .section-divider { height:1px; background:#eef0f3; margin:6px 0 20px 0; }
    .placeholder-msg { color:#aab4be; font-size:13px; font-style:italic; }
  "))),
  
  div(class = "app-header",
      tags$img(src = "PEOPLE_Ecosystems_Conservation_key visual.jpg",
               alt = "PEOPLE-ECCO logo"),
      div(class = "app-header-text",
          h2("PEOPLE-ECCO BACI"),
          p("Supported by ESA")
      )
  ),
  
  tabsetPanel(
    id = "main_tabs",
    
    # -- Tab 1: Instructions --------------------------------------------------
    tabPanel(
      title = "Instructions",
      h3("Instructions"),
      p("General overview of solution, links to user handbook, etc.")
    ),
    
    # -- Tab 2: Vector input & Matching covariates ---------------------------
    tabPanel(
      title = "Vector & Covariates",
      
      h3("Vector input & Matching covariates"),
      
      # -- Section 1: Vector file input + attribute preview ------------------
      div(class = "card",
          div(class = "card-title", span(class = "icon", "\U0001f4c2"), "Vector Input"),
          p(style = "font-size:13px; color:#666; margin-bottom:18px;",
            "Upload a vector file from disk. Allowed formats: GeoJSON or Shapefile
           (select all associated files for Shapefile)."),
          fileInput(
            inputId     = "geojson_file",
            label       = "Browse for a vector file",
            accept      = c(".geojson", ".json", ".shp", ".dbf", ".shx", ".prj", ".cpg", ".qpj"),
            multiple    = TRUE,
            buttonLabel = "Browse\u2026",
            placeholder = "No file selected"
          ),
          uiOutput("geom_type_ui")
      ),
      
      fluidRow(
        column(5,
               uiOutput("select_ci_card_ui"),
               uiOutput("attr_card_ui")
        ),
        column(7,
               uiOutput("vector_plot_card_ui")
        )
      ),
      
      # -- Section 2: Matching covariates ------------------------------------
      div(style = "margin-top:8px;",
          h4("Matching covariates"),
          p(style = "font-size:13px; color:#666;",
            "Select or load the covariate layers to use in matching.")
      ),
      
      div(class = "card",
          div(class = "card-title",
              span(class = "icon", "\U0001f4c1"),
              "From input vector"
          ),
          p(style = "font-size:13px; color:#666; margin-bottom:18px;",
            "Select attributes already present in the input vector file to use
           as matching covariates."),
          uiOutput("matchvars_select_ui")
      ),
      
      div(class = "card",
          div(class = "card-title",
              span(class = "icon", "\U0001f4be"),
              "From additional sources"
          ),
          p(style = "font-size:13px; color:#666; margin-bottom:20px;",
            "Add additional covariate layers from external sources. Select the
           source type first; the relevant options appear below. After each
           source is confirmed a new entry appears automatically. Files are
           registered by path only and not loaded into memory at this stage."),
          uiOutput("matchlyr_rows_ui")
      ),
      
      uiOutput("extract_btn_ui")
    ),
    
    
    # -- Tab 3: Matching analysis --------------------------------------------
    tabPanel(
      title = "Matching analysis",
      
      h3("Matching analysis"),
      p("Set up the matching dataset, configure parameters, run matching,
         and evaluate results."),
      
      # -- Section 1: Input & parameters ------------------------------------
      fluidRow(
        column(5,
               div(class = "card",
                   div(class = "card-title",
                       span(class = "icon", "\U0001f4c2"),
                       "Matching dataset"
                   ),
                   p(style = "font-size:13px; color:#666; margin-bottom:16px;",
                     "Provide the SpatVector containing treatment indicator and
               matching covariates. Use the output from the Vector & Covariates
               tab, or load an existing file from disk."),
                   radioButtons("match_input_source",
                                label    = NULL,
                                choices  = c(
                                  "Use output from Vector & Covariates tab" = "from_tab",
                                  "Load from file"                          = "from_file"
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
          h4("Matching evaluation"),
          p(style = "font-size:13px; color:#666;",
            "Review matched units, assess balance, and save results.")
      ),
      
      uiOutput("match_eval_dropped_card_ui"),
      uiOutput("match_eval_map_card_ui"),
      uiOutput("match_eval_attr_card_ui"),
      uiOutput("match_eval_diag_card_ui"),
      uiOutput("match_eval_save_card_ui")
    ),
    
    # -- Tab 6: Impact evaluation ---------------------------------------------
    tabPanel(
      title = "Impact evaluation",
      
      h3("Impact evaluation"),
      p("Some info about this tab."),
      
      fluidRow(
        column(5,
               div(class = "card",
                   div(class = "card-title",
                       span(class = "icon", "\U0001f4c2"),
                       "Matched control-impact pairs"
                   ),
                   p(style = "font-size:13px; color:#666; margin-bottom:16px;",
                     "Provide the SpatVector of matched control-impact pairs. Use
               the output from the Matching evaluation tab, or load an
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
    ),
    
    
    
  )  # end tabsetPanel
)  # end fluidPage