# Table of Contents
# last update 2025-05-26

#' This ToC links to all code used in SM data collection, wrangling, metadata 
#' creation, and export to sm-docs and sm-explorer repos. To navigate to a 
#' script, use `F2` or `CTRL + LEFT CLICK` on the path to use the definition
#' function.

#' Note that all scripts use the projecter package. It should be automatically
#' installed in the .Rprofile script, but can be finicky. If it is not loaded,
#' use the following to download from GitHub:

# if (!require('remotes')) install.packages('remotes')
# remotes::install_github('ChrisDonovan307/projecter')
# library('projecter')



# Housekeeping ------------------------------------------------------------


# Load some project management functions
source('3_functions/project_management.R')

# Prep keys to NE state and county fips codes, also county spatial layers
source('4_scripts/spatial_wrangling.R')

# Wrangle bulk download file from NASS 2022 Census of Agriculture
source('4_scripts/nass_bulk_download.R')



# Workflow ----------------------------------------------------------------
## API Calls ---------------------------------------------------------------


# Note that some API calls are in scripts with cleaning. 
# To Do: pull them out and compile them here.

# Eventually this will house all the API calls separately. Raw API responses
# will be kept in 5_objects/api_outs/, then the wrangle scripts will take those
# responses and clean them and save them properly. 

# Pulling NASS data from API
'4_scripts/nass_api_call.R'



## Wrangle Data ------------------------------------------------------------


# These scripts take data from 1_raw/ (or 5_objects in the case of API calls),
# pull and wrangle relevant metrics from them, and create associated metadata
# files. Metrics are saved in long format to 5_objects/metrics/ and metadata
# is saved in wide format to 5_objects/metadata/ for each script. Scripts are
# organized somewhat thematically, or sometimes functionally as seemed 
# appropriate at the time, even though it might no longer be appropriate.

# Cleaning and compiling NASS data from API
source('4_scripts/nass_compilation.R')


# Pull existing data from USDA ARMS Data Warehouse. Note that while we started
# out leaning on this data source, we have since moved on to pulling from the
# raw data sources, as it is not clear if or when the data warehouse will be
# updated.
source('4_scripts/data_warehouse.R')

# Pull census data (ACS5). [Switching from bulk download to API calls]
# Note - need to move API calls in different script, save raw outputs
# Note: Separate API calls
source('4_scripts/census.R')

# BLS QCEW - https://www.bls.gov/cew/
# Note: Separate API calls
source('4_scripts/bls_api.R')

# FDA API for recalls, USDM api for droughts. Note to separate API calls
# Note: Separate API calls
source('4_scripts/fda_usdm_apis.R')

# County health rankings from University of Wisconsin
# https://www.countyhealthrankings.org/
source('4_scripts/county_health_rankings.R')

# Other - EPA, USDA bee surveys, FSA disaster declarations, ERS farm income and
# wealth, BEA GDP by industry
source('4_scripts/other_datasets.R')

# Spatial data - MRLC, VT BioD Proj, USFS Treemap. Note that this script has a
# long run time, and needs a rework or two.
source('4_scripts/lulc.R')



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


# Explore refined framework resolution
'4_scripts/refined_framework.R'

# Get parameter values for NASS API - saved into object for reference.
'4_scripts/get_param_options.R'

# Explore bulk download from USDA ARMS. ARMS isn't helpful for us though.
'4_scripts/arms.R'

# USDA ERS Food Environment Atlas
'4_scripts/usda_food_environment_atlas.R'

# Sandbox for temporary work
'4_scripts/sandbox.R'

