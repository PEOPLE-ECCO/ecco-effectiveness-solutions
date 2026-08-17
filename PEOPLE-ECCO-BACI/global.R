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


# Increase maximum file upload size (default is 5MB)
options(shiny.maxRequestSize = 50 * 1024^2)   # 50 MB

