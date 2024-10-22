#' Aggregate Data
#' 2024-09-26

#' Combine data from NASS and Data Warehouse, others sources, into one file


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr
)

source('3_functions/read_all_rds.R')
source('3_functions/data_pipeline_functions.R')

# Load datasets
dat <- read_all_rds('5_objects/metrics/')  
get_str(dat)

# Load metadata
meta <- read_all_rds('5_objects/metadata/')
get_str(meta)



# Aggregate Metrics --------------------------------------------------------


map(dat, get_str)

# Keep only fips, year, variable_name, value
agg <- map(dat, ~ {
  .x %>% 
    select(fips, year, variable_name, value) %>% 
    mutate(across(c(year, value), as.numeric))
  }) %>% 
  bind_rows()
get_str(agg)

# Explore
agg$variable_name %>% 
  unique %>% 
  sort

# Save this as clean dataset for use in docs and app. Also csv
saveRDS(agg, '2_clean/metrics.rds')
write_csv(agg, '6_outputs/metrics.csv')



# Metadata ----------------------------------------------------------------


map(meta, get_str)

# Combine them, warehouse is FALSE if not inluded
meta_agg <- bind_rows(meta) %>% 
  mutate(warehouse = ifelse(is.na(warehouse), FALSE, warehouse))
get_str(meta_agg)

# Save this as clean set
saveRDS(meta_agg, '2_clean/metadata.rds')
write_csv(meta_agg, '6_outputs/metadata.csv')