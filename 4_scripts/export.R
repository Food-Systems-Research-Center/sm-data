# Export data to SMquarto and SMexplorer
# 2024-10-14



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr
)

source('3_functions/read_all_rds.R')



# Load Data ---------------------------------------------------------------


# Initiate list
out <- list()

# Metrics and metadata table
out$metrics <- read_all_rds(path = '2_clean/', pattern = '^met')

# Spatial objects and references
out$fips <- read_all_rds(path = '5_objects/', pattern = '^fips')
out$counties <- read_all_rds('2_clean/spatial/', pattern = '^ne_counties_')

# get_str(out)
out <- list_flatten(out, name_spec = "{inner}")
get_str(out)



# Export Data -------------------------------------------------------------


# Paths to SMquarto and SMexplorer
saveRDS(out, '../SMquarto/data/sm_data.rds')
saveRDS(out, '../SMexplorer/dev/data/sm_data.rds')
