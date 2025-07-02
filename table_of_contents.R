# Table of Contents
# last update 2025-06-15

#' This ToC links to all code used in SM data collection, wrangling, metadata 
#' creation, and export to sm-docs and sm-explorer repos. To navigate to a 
#' script, use `F2` or `CTRL + LEFT CLICK` on the path to use the definition
#' function.

#' Note that the `setup.R` script must be run before anything else in the 
#' project, as it loads the projecter package with convenience functions use
#' throughout the project, as well as the SMdata package, which provides 
#' utility functions.



# Housekeeping ------------------------------------------------------------


# Load SMdata project, essential packages
source('3_functions/setup.R')

# Prep keys to NE state and county fips codes, also county spatial layers
source('4_scripts/spatial_wrangling.R')

# Make a key for NAICS codes used in NASS and BLS
source('4_scripts/naics_key.R')



# Workflow ----------------------------------------------------------------
## API Calls ---------------------------------------------------------------


# Scripts with API calls are kept here. Raw API responses are in
# 5_objects/api_outs/, then the wrangle scripts below will take those responses
# and clean them and save them properly.

# Removing source() call to prevent accidental runs. Call using sm_call_api()
'4_scripts/api_calls/nass_api.R'
'4_scripts/api_calls/bls_api.R'
'4_scripts/api_calls/census_api.R'
'4_scripts/api_calls/fda_api.R'
'4_scripts/api_calls/usdm_api.R'



## Wrangle Data ------------------------------------------------------------


# These scripts take data from 1_raw/ (or 5_objects in the case of API calls),
# pull and wrangle relevant metrics from them, and create associated metadata
# files. Metrics are saved in long format to 5_objects/metrics/ and metadata
# is saved in wide format to 5_objects/metadata/ for each script. Scripts are
# organized somewhat thematically, or sometimes functionally as seemed 
# appropriate at the time, even though it might no longer be appropriate.

# Cleaning and compiling NASS data from API
source('4_scripts/nass.R')

# Pull census data (ACS5)
source('4_scripts/census.R')

# BLS QCEW and ERS farm and income statistics
source('4_scripts/bls_ers.R')

# County health rankings from University of Wisconsin
source('4_scripts/county_health_rankings.R')

# Feeding America - Map the Meal Gap
source('4_scripts/map_meal_gap.R')

# Other - EPA, USDA bee surveys, FSA disaster declarations, BEA GDP by industry,
# FDA recall enforcement, USDM drought
source('4_scripts/other_datasets.R')

# Spatial data - MRLC LULC, USFS Treemap. Note that this script has a long run
# time
source('4_scripts/spatial.R')



## Export ------------------------------------------------------------------


# These scripts take the metrics data, metadata, and all associated files and
# datasets, like framework trees for graphs, helper datasets like fips keys,
# spatial data like counties and states, does some last minute checks, and puts
# them all together into two rds list objects. Then the export script sends 
# those to sm-docs and sm-explorer

# Combine metrics, metadata, run some checks
source('4_scripts/aggregate_data.R')

# Export data to sm-docs and sm-explorer
source('4_scripts/export.R')

# Explore aggregated data
'4_scripts/explore.R'



# Miscellany --------------------------------------------------------------


# Get parameter values for NASS API - saved into object for reference.
'4_scripts/get_param_options.R'

# Explore bulk download from USDA ARMS. ARMS isn't helpful for us though.
'4_scripts/arms.R'

# USDA ERS Food Environment Atlas
'4_scripts/usda_food_environment_atlas.R'

# Sandbox for temporary work
'4_scripts/sandbox.R'


## Random scripts for working on trees for data paper
'4_scripts/pull_from_excel.R'