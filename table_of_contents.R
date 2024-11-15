# Table of Contents


# Housekeeping ------------------------------------------------------------


# Prep keys to NE state and county fips codes, also county spatial layers
source('4_scripts/spatial_wrangling.R')

# Wrangle bulk download file from NASS 2022 Census of Agriculture
source('4_scripts/nass_bulk_download.R')



# Main Workflow -----------------------------------------------------------


## Collect Data -----
# Pulling NASS data from API, then aggregating, then cleaning and compiling
'4_scripts/nass_api_call.R'
source('4_scripts/nass_api_wrangle.R')
source('4_scripts/nass_compilation.R')

# Pull existing data from USDA ARMS Data Warehouse
source('4_scripts/data_warehouse.R')

# Pull census data (ACS5). [Switching from bulk download to API calls]
source('4_scripts/census.R')

# BLS QCEW
source('4_scripts/bls_api.R')

# Other datasets. Mostly EPA so far
source('4_scripts/other_datasets.R')


## Aggregate and Export -----
# Combine data from all sources
source('4_scripts/aggregate_data.R')

# Export data to sm-docs and sm-explorer
source('4_scripts/export.R')

# Explore aggregated data
'4_scripts/explore.R'



# Other datasets ----------------------------------------------------------


# VT Open Geodataportal Base Land Cover 2022
'4_scripts/lulc.R'



# Miscellany --------------------------------------------------------------


# Get parameter values - saved into object for reference.
source('4_scripts/get_param_options.R')

# Explore bulk download from USDA ARMS. ARMS isn't helpful for us though.
source('4_scripts/arms.R')

# Sandbox
'4_scripts/sandbox.R'