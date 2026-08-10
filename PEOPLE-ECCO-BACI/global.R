library(shiny)
library(bslib)
library(terra)
library(leaflet)
library(sf)
library(data.table)

source("R/extract_covariates.R")
source("R/check_multicollinearity.R")
source("R/run_matching.R")
source("R/evaluate_matching.R")
source("R/run_impact_assessment.R")

if (requireNamespace("ragg", quietly = TRUE)) {
  options(shiny.useragg = TRUE)
}

