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

if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  dplyr,
  conflicted,
  devtools
)

pacman::p_load_gh('ChrisDonovan307/projecter')

devtools::load_all()


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

