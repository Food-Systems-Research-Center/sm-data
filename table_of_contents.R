# Table of Contents
# 2024-09-10


# Housekeeping ------------------------------------------------------------



# Main Workflow -----------------------------------------------------------


# Wrangle bulk download file from NASS 2022 Census of Agriculture
# Also get county level spatial data
source('4_scripts/nass_bulk_download.R')
source('4_scripts/nass_wrangling.R')

# Pull existing data from USDA ARMS Data Warehouse
source('4_scripts/data_warehouse.R')

# Test out census API
source('4_scripts/census_api.R')

# Combine them
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


# Get parameter values - saved into object for reference
source('4_scripts/get_param_options.R')

# NASS data from API - not doing this though. Bulk makes more sense
source('4_scripts/pull_NASS_data.R')

# Explore bulk download from USDA ARMS
source('4_scripts/arms.R')