# Export data to SMquarto and SMexplorer
# 2024-10-14



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr,
  stars,
  sf
)

source('3_functions/pipeline_utilities.R')



# Load Data ---------------------------------------------------------------


# Initiate list
out <- list()

# Metrics and metadata table
out$metrics <- read_all_rds(path = '2_clean/', pattern = '^met')

# Spatial objects and references
out$fips <- read_all_rds(path = '5_objects/', pattern = '_key.rds$')
out$counties_and_states <- read_all_rds(
  '2_clean/spatial/', 
  pattern = '(?i)^ne_.*.rds|^all_.*.rds'
)

# Map layers
out$spatial <- read_all_rds('2_clean/spatial/map_layers/', pattern = '.rds$')

# Refined tree structure for new framework
out$refined_tree <- read.csv('2_clean/trees/refined_secondary_tree.csv')

# Flatten into single layer list
out <- list_flatten(out, name_spec = "{inner}")

get_str(out)



# Export Data -------------------------------------------------------------


# Paths to SMquarto and SMexplorer. Also to SMdata to load into DB
paths <- c(
  '../sm-docs/data/sm_data.rds',
  '../sm-explorer/dev/data/sm_data.rds',
  '6_outputs/sm_data.rds'
)
walk(paths, ~ saveRDS(out, .x))
cat('\nExported data to:\n', paths[1], '\n', paths[2], sep = '')

clear_data()
