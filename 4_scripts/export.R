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
out$fips <- read_all_rds(path = '5_objects/', pattern = '_key.rds$')
out$counties <- read_all_rds('2_clean/spatial/', pattern = '^ne_')

# Flatten into single layer list
out <- list_flatten(out, name_spec = "{inner}")
<<<<<<< Updated upstream
str(out)
names(out)
=======
get_str(out)

out$metrics %>% 
  filter(
    variable_name == 'annualAvgEstabs111NAICS',
    fips == '25009'
  )
>>>>>>> Stashed changes


# Export Data -------------------------------------------------------------


# Paths to SMquarto and SMexplorer
saveRDS(out, '../sm-docs/data/sm_data.rds')
saveRDS(out, '../sm-explorer/dev/data/sm_data.rds')

clear_data()