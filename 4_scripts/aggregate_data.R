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
source('3_functions/check_n_records.R')

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
    mutate(across(c(year, value), as.character))
  }) %>% 
  bind_rows()
get_str(agg)

# Explore
agg$variable_name %>% 
  unique %>% 
  sort
# Note that our variable count is fucked now that were have NAICS codes 

# Make latest_year column 

# Metadata ----------------------------------------------------------------


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



# Check and save ----------------------------------------------------------


# Check to make sure we have the same number of metrics and metas
try(check_n_records(agg, meta_agg, 'Aggregation'))
print('NAICS issues cause discrepancy of 10')

# Save metrics as clean dataset for use in docs and app. Also csv
saveRDS(agg, '2_clean/metrics.rds')
write_csv(agg, '6_outputs/metrics.csv')

# Save meta as clean set
saveRDS(meta_agg, '2_clean/metadata.rds')
write_csv(meta_agg, '6_outputs/metadata.csv')

clear_data()
