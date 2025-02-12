# Table of Contents


# Housekeeping ------------------------------------------------------------


# Prep keys to NE state and county fips codes, also county spatial layers
source('4_scripts/spatial_wrangling.R')

# Wrangle bulk download file from NASS 2022 Census of Agriculture
source('4_scripts/nass_bulk_download.R')



# Workflow ----------------------------------------------------------------
## API Calls ---------------------------------------------------------------


# Note that some API calls are in scripts with cleaning. To Do: pull them out
# and compile them here.

# Pulling NASS data from API
'4_scripts/nass_api_call.R'



## Wrangle Data ------------------------------------------------------------


# Cleaning and compiling NASS data from API
source('4_scripts/nass_compilation.R')

# Pull existing data from USDA ARMS Data Warehouse
source('4_scripts/data_warehouse.R')

# Pull census data (ACS5). [Switching from bulk download to API calls]
source('4_scripts/census.R')

# BLS QCEW
source('4_scripts/bls_api.R')

# FDA API for recalls, USDM api for droughts
source('4_scripts/fda_usdm_apis.R')

# County health rankings
source('4_scripts/county_health_rankings.R')

# Other - EPA, USDA bee surveys, FSA disaster declarations, ERS farm income and
# wealth
source('4_scripts/other_datasets.R')

# Spatial data - MRLC, VT BioD Proj, USFS Treemap (long run time)
source('4_scripts/lulc.R')



## Export ------------------------------------------------------------------


# Combine data from all sources
source('4_scripts/aggregate_data.R')

# Export data to sm-docs and sm-explorer
source('4_scripts/export.R')

# Explore aggregated data
'4_scripts/explore.R'



# Miscellany --------------------------------------------------------------


# Explore refined framework resolution
source('4_scripts/refined_framework.R')

# Get parameter values - saved into object for reference.
source('4_scripts/get_param_options.R')

# Explore bulk download from USDA ARMS. ARMS isn't helpful for us though.
source('4_scripts/arms.R')

# USDA ERS Food Environment Atlas
source('4_scripts/usda_food_environment_atlas.R')

# Sandbox
'4_scripts/sandbox.R'

