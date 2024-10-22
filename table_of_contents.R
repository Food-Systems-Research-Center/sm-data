# Table of Contents
# 2024-09-10


# Housekeeping ------------------------------------------------------------


# Prep keys to NE state and county fips codes, also county spatial layers
source('4_scripts/spatial_wrangling.R')

# Wrangle bulk download file from NASS 2022 Census of Agriculture
source('4_scripts/nass_bulk_download.R')



# Main Workflow -----------------------------------------------------------


## Collect Data -----
# Pull relevant NASS variables and compile them
source('4_scripts/nass_wrangling.R')

# Pull existing data from USDA ARMS Data Warehouse
source('4_scripts/data_warehouse.R')

# Pull census data (ACS5). [Process of switching from bulk to API calls]
source('4_scripts/census.R')


## Aggregate Data -----
# Combine data from all sources
source('4_scripts/aggregate_data.R')

# Explore aggregated data
'4_scripts/explore.R'

# Export data to SMquarto and SMexplorer
source('4_scripts/export.R')



# Other datasets ----------------------------------------------------------


# EPA GHG inventory explorer
source('4_scripts/epa_ghg_data.R')

# VT Open Geodataportal Base Land Cover 2022
'4_scripts/lulc.R'



# Miscellany --------------------------------------------------------------


# Get parameter values - saved into object for reference.
source('4_scripts/get_param_options.R')

# NASS data from API. Should revisit this instead of using bulk download above.
source('4_scripts/pull_NASS_data.R')

# Explore bulk download from USDA ARMS. ARMS isn't helpful for us though.
source('4_scripts/arms.R')