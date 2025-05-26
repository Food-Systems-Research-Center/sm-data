# Project Management
# 2025-2-15


# Description -------------------------------------------------------------

# Collection of functions for navigating repository, automating wrangling,
# aggregation, and exports. Mostly just sourcing scripts. One of these days
# we should convert it into a more function oriented workflow though


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr
)


# sm_apis -----------------------------------------------------------------

# Waiting on this one until we clean up our API calls properly.


# sm_wrangle --------------------------------------------------------------

# Note that we are not currently including the LULC wrangling script here
# because the run time is pretty long. Should reduce it to just what we need
# and add it
sm_wrangle <- function() {
  source('4_scripts/nass_compilation.R')
  source('4_scripts/data_warehouse.R')
  source('4_scripts/census.R')
  source('4_scripts/bls_api.R')
  source('4_scripts/fda_usdm_apis.R')
  source('4_scripts/county_health_rankings.R')
  source('4_scripts/other_datasets.R')
}


# sm_aggregate ------------------------------------------------------------

sm_aggregate <- function() {
  source('4_scripts/aggregate_data.R')
  source('4_scripts/export.R')
}


# sm_export ---------------------------------------------------------------

sm_export <- function() {
  source('4_scripts/export.R')
}


# Message -----------------------------------------------------------------


# Print a message showing functions, options
cat(
  '\n*Loaded project management functions*',
  '\n sm_wrangle(): Run all wrangle scripts from raw data or API outs',
  '\n sm_aggregate(): Combine all wrangled metrics and metadata, then run checks on counts',
  '\n sm_export(): Send metrics, metadata, spatial, to 6_outputs, sm-docs, sm-explorer\n'
)
  
