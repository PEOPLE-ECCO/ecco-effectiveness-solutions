# Source tab UI definitions
source("tabs/tab2_covariates_ui.R")
source("tabs/tab3_matching_ui.R")
source("tabs/tab4_impact_ui.R")

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
      includeMarkdown("instructions.md")
    ),

    tab2_ui,
    tab3_ui,
    tab4_ui

  )
)
