# Refined Framework
# 2025-01-07


# Description -------------------------------------------------------------

# Figure out how hard it will be to get state date for the rest of our metrics,
# or across states


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr
)

raw <- read.csv('2_clean/trees/refined_secondary_tree.csv')
meta <- readRDS('2_clean/metadata.rds')



# Explore -----------------------------------------------------------------

raw$resolution %>% unique
raw$resolution %>% get_table()

# Meta subset - relevant vars
meta_subset <- meta %>% 
  select(variable_name, resolution, scope, source)

# Check metrics where we only have county data
county_only <- raw %>% 
  select(variable_name) %>% 
  left_join(meta_subset, by = 'variable_name') %>% 
  select(variable_name, resolution, source) %>% 
  filter(resolution == 'county')
county_only

county_only_names <- county_only %>% 
  pull(variable_name)
county_only_names
