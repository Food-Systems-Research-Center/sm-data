# Export data to sm-docs and sm-explorer
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
out$fips <- read_all_rds(path = '5_objects/', pattern = '_key.rds$|^all_fips')

# Refined tree structure for new framework
# For placeholders (metric == 'none'), give them unique numbers
tree <- read.csv('2_clean/trees/refined_secondary_tree.csv')
count <- sum(tree$metric == 'NONE')
tree$metric[tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
out$refined_tree <- tree

# Add fixed refined tree with all the indicators
fixed_tree <- read.csv('2_clean/trees/fixed_tree.csv')
count <- sum(fixed_tree$metric == 'NONE')
fixed_tree$metric[fixed_tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
out$fixed_tree <- fixed_tree

# Also add new tree (reducing metrics for RFPP and Frontiers)
new_tree <- read.csv('2_clean/trees/new_tree.csv')
count <- sum(new_tree$metric == 'NONE')
new_tree$metric[new_tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
out$new_tree <- new_tree

# And conference tree...
conference_tree <- read.csv('2_clean/trees/conference_tree.csv')
count <- sum(conference_tree$metric == 'NONE')
conference_tree$metric[conference_tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
out$conf_tree <- conference_tree


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

# (?i) is case insensitive
spatial$counties_and_states <- read_all_rds(
  '2_clean/spatial/',
  pattern = '(?i)^neast_.*.rds|^all_states.*.rds'
)

# Flatten into single level list
spatial <- list_flatten(spatial, name_spec = "{inner}")
get_str(spatial)



# Export Data -------------------------------------------------------------


# Paths to SMquarto and SMexplorer. Also to SMdata to load into DB
paths <- c(
  '../SMdocs/data/sm_data.rds',
  '../SMexplorer/dev/data/sm_data.rds',
  '6_outputs/sm_data.rds'
)
walk(paths, ~ saveRDS(out, .x))

# Save spatial data in those places also
spatial_paths <- c(
  '../SMdocs/data/sm_spatial.rds',
  '../SMexplorer/dev/data/sm_spatial.rds',
  '6_outputs/sm_spatial.rds'
)
walk(spatial_paths, ~ saveRDS(spatial, .x))

# Also save a csv of metrics_df only for bulk downloads
write_csv(out$metrics, '../SMdocs/data/bulk_metrics.csv')

# Clear, memory, out message
clear_data()
gc()
cat('\n*Exports complete*')
