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


  # ---------------------------------------------------------------------------
  # Tab server logic is split into separate files for maintainability.
  # local=TRUE ensures each file runs in the server() environment and has
  # access to input, output, session, and all reactives defined here.
  # ---------------------------------------------------------------------------
  source("tabs/tab2_covariates_server.R", local = TRUE)
  source("tabs/tab3_matching_server.R",   local = TRUE)
  source("tabs/tab4_impact_server.R",     local = TRUE)

}
