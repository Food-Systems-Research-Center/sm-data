# Setup
# 2025-06-30


# Description -------------------------------------------------------------

# Collection of functions for navigating repository, automating wrangling,
# aggregation, and exports. Mostly just sourcing scripts. One of these days
# we should convert it into a more function oriented workflow though

# Also loading some packages used throughout the project, including projecter(),
# which contains convenience functions like get_str()

# Also loading the SMdata package itself. Should combine these project
# management functions into the package at some point.


# Housekeeping ------------------------------------------------------------

# Install package manager and load critical packages
if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  conflicted,
  devtools
)

# Load package of convenience functions used throughout project
pacman::p_load_gh('ChrisDonovan307/projecter')

# Load SMdata package, making functions and datasets available
# this includes internal functions for use in data wrangling
devtools::load_all()


# sm_call_apis ------------------------------------------------------------

# Function that calls api scripts. Can call one at a time or all at once.
sm_call_apis <- function(nass = FALSE,
                         bls = FALSE,
                         census = FALSE,
                         fda = FALSE,
                         usdm = FALSE,
                         all = FALSE) {
  if (all) {
    nass <- TRUE
    bls <- TRUE
    census <- TRUE
    fda <- TRUE
    usdm <- TRUE
  }
  if (nass) source('4_scripts/api_calls/nass_api.R')
  if (bls) source('4_scripts/api_calls/bls_api.R')
  if (census) source('4_scripts/api_calls/census_api.R')
  if (fda) source('4_scripts/api_calls/fda_api.R')
  if (usdm) source('4_scripts/api_calls/usdm_api.R')
}


# sm_wrangle --------------------------------------------------------------

# Note that we are not currently including the LULC wrangling script here
# because the run time is pretty long. Should reduce it to just what we need
# and add it. Or maybe make it a separate function so we only run it when we
# really mean to
sm_wrangle <- function() {
  source('4_scripts/nass_compilation.R')
  source('4_scripts/census.R')
  source('4_scripts/bls_ers.R')
  source('4_scripts/county_health_rankings.R')
  source('4_scripts/map_meal_gap.R')
  source('4_scripts/other_datasets.R')
  # source('4_scripts/lulc.R')
}


# sm_aggregate ------------------------------------------------------------

sm_aggregate <- function() {
  source('4_scripts/project/aggregate_data.R')
}


# sm_export ---------------------------------------------------------------

sm_export <- function() {
  source('4_scripts/project/export.R')
}


# Message -----------------------------------------------------------------

# Print a message showing functions, options
cat(
  '\n*Loaded project management functions*',
  '\n sm_wrangle(): Run all wrangle scripts from raw data or API outs',
  '\n sm_aggregate(): Combine all wrangled metrics and metadata, then run checks on counts',
  '\n sm_export(): Send metrics, metadata, spatial, to 6_outputs, sm-docs, sm-explorer\n'
)

