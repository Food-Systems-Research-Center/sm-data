#' Aggregate Data
#' 2024-09-26

#' Combine data from NASS and Data Warehouse, other sources, into one file


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr
)

source('3_functions/pipeline_utilities.R')
source('3_functions/metadata_utilities.R')

# Load datasets
dat <- read_all_rds('5_objects/metrics/')  
get_str(dat)

# Load metadata
meta <- read_all_rds('5_objects/metadata/')
get_str(meta)



# Aggregate Metrics --------------------------------------------------------


map(dat, get_str)

# Keep all the weird variables, cv percent, disclosure, value codes, margins
# Just make sure year is numeric
agg <- map(dat, ~ {
  .x %>% 
    mutate(across(c(year, value), as.character)) %>% 
    select(-any_of(c('metric', 'county_name')))
  }) %>% 
  bind_rows() %>% 
  select(fips, year, variable_name, value)
get_str(agg)

# Explore
agg$variable_name %>% 
  unique %>% 
  sort
# Note that our variable count is fucked now that were have NAICS codes 

# NOTE: we used to remove these, but now let's keep them in. Want them for 
# income stability and 
# agg <- agg %>% 
#   filter(str_detect(
#     variable_name,
#     regex('NAICS|^lq|^avgEmpLvl', ignore_case = TRUE),
#     negate = TRUE
#   ))



# Metadata ----------------------------------------------------------------


get_str(meta)
map(meta, get_str)

# Combine them, warehouse is FALSE if not included
meta_agg <- bind_rows(meta) %>% 
  mutate(warehouse = ifelse(is.na(warehouse), FALSE, warehouse))
get_str(meta_agg)

# Also add latest year column 
# Also convert '|' to commas and spaces
meta_agg <- meta_agg %>% 
  mutate(
    year = str_replace_all(year, '\\|', ', '),
    latest_year = map_chr(year, ~ {
      .x %>% 
        str_split_1(', ') %>% 
        max()
      })
  )
meta_agg$latest_year
meta_agg$year

# If there is no axis name, make it the variable_name
meta_agg <- meta_agg %>% 
  mutate(axis_name = ifelse(is.na(axis_name), variable_name, axis_name))

# Remove NAICS variables here too
# JUST KIDDING keep them in
meta_agg$variable_name %>% 
  unique %>% 
  sort

# meta_agg <- meta_agg %>% 
#   filter(str_detect(
#     variable_name,
#     regex('NAICS|^lq|^avgEmpLvl', ignore_case = TRUE),
#     negate = TRUE
#   ))
# meta_agg$variable_name %>% 
#   unique %>% 
#   sort



# Check and save ----------------------------------------------------------


# Check to make sure we have the same number of metrics and metas
try(check_n_records(agg, meta_agg, 'Aggregation'))

# Save metrics as clean dataset for use in docs and app. Also csv
saveRDS(agg, '2_clean/metrics.rds')
write_csv(agg, '6_outputs/metrics.csv')

# Save meta as clean set
saveRDS(meta_agg, '2_clean/metadata.rds')
write_csv(meta_agg, '6_outputs/metadata.csv')

clear_data()
gc()

cat('\n*Aggregation complete*')