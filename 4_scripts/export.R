# Export data to SMquarto and SMexplorer
# 2024-10-14



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr,
  stars,
  sf,
  readr
)

source('3_functions/pipeline_utilities.R')



# Load Data ---------------------------------------------------------------


# Initiate list
out <- list()

# Metrics and metadata table
out$metrics <- read_all_rds(path = '2_clean/', pattern = '^met')

# Spatial objects and references
out$fips <- read_all_rds(path = '5_objects/', pattern = '_key.rds$')

# Refined tree structure for new framework
# For placeholders (metric == 'none'), give them unique numbers
tree <- read.csv('2_clean/trees/refined_secondary_tree.csv')
count <- sum(tree$metric == 'NONE')
tree$metric[tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
out$refined_tree <- tree

# Flatten into single layer list
out <- list_flatten(out, name_spec = "{inner}")

get_str(out)



# Spatial Data ------------------------------------------------------------


# Separating all spatial files from tabular files
spatial <- list()

# Map layers
spatial$spatial <- read_all_rds('2_clean/spatial/map_layers/', pattern = '.rds$')

# County and state layers
# NOTE: all counties files are over 100MB and cannot be easily pushed on GitHub
# We are dropping these for now - don't actually use them yet.

# Old code here to pull everything:
# spatial$counties_and_states <- read_all_rds(
#   '2_clean/spatial/', 
#   pattern = '(?i)^ne_.*.rds|^all_.*.rds'
# )

# Pull everything except the national counties polygons (only new england)
spatial$counties_and_states <- read_all_rds(
  '2_clean/spatial/',
  pattern = '(?i)^ne_.*.rds|^all_states.*.rds'
)

# Flatten into single level list
spatial <- list_flatten(spatial, name_spec = "{inner}")
get_str(spatial)



# Export Data -------------------------------------------------------------


# Paths to SMquarto and SMexplorer. Also to SMdata to load into DB
paths <- c(
  '../sm-docs/data/sm_data.rds',
  '../sm-explorer/dev/data/sm_data.rds',
  '6_outputs/sm_data.rds'
)
walk(paths, ~ saveRDS(out, .x))

# Save spatial data in those places also
spatial_paths <- c(
  '../sm-docs/data/sm_spatial.rds',
  '../sm-explorer/dev/data/sm_spatial.rds',
  '6_outputs/sm_spatial.rds'
)
walk(spatial_paths, ~ saveRDS(spatial, .x))

# Also save a csv of metrics_df only for bulk downloads
write_csv(out$metrics, '../sm-docs/data/bulk_metrics.csv')

# Clear, memory, out message
clear_data()
gc()
cat('\n*Exports complete*')
