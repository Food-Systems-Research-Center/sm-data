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
metrics <- readRDS('2_clean/metrics.rds')



# Explore -----------------------------------------------------------------

# Meta subset - relevant vars
meta_subset <- meta %>% 
  select(variable_name, resolution, scope, source)

# Check metrics where we only have county data
county_only <- raw %>% 
  filter(use == 'x') %>% 
  select(variable_name) %>% 
  left_join(meta_subset, by = 'variable_name') %>% 
  select(variable_name, resolution, source) %>% 
  filter(resolution == 'county')
county_only

good_vars <- raw %>% 
  filter(use == 'x') %>% 
  select(variable_name) %>% 
  left_join(meta_subset, by = 'variable_name') %>% 
  select(variable_name, resolution, source)
good_vars
good_vars$source

vars <- good_vars$variable_name  
missing_indices <- which(! vars %in% meta$variable_name)
missing_vars <- vars[missing_indices]
missing_vars

# What the fuck is going on
all_vars <- get_vars(metrics)
missing_vars %in% all_vars
str_subset(all_vars, 'methane')

meta %>% 
  filter(str_detect(variable_name, '^sales')) %>% 
  select(variable_name, metric)


# Check if it works -------------------------------------------------------


good_vars <- raw %>% 
  dplyr::filter(use == 'x') %>% 
  select(variable_name) %>% 
  left_join(meta_subset, by = 'variable_name') %>% 
  select(variable_name, resolution, source)
good_vars

good_var_names <- good_vars$variable_name
good_var_names

# Save these somewhere
saveRDS(good_var_names, '2_clean/refined_var_names.RDS')
