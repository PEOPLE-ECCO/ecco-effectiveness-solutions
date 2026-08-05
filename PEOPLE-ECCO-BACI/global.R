library(shiny)
library(bslib)
library(terra)
library(leaflet)
library(sf)

source("R/extract_covariates.R")
source("R/check_multicollinearity.R")
source("R/run_matching.R")
source("R/evaluate_matching.R")

if (requireNamespace("ragg", quietly = TRUE)) {
  options(shiny.useragg = TRUE)
}

# Limit terra's RAM use and allow disk spill for large rasters
terra::terraOptions(memfrac = 0.4, tempdir = tempdir())