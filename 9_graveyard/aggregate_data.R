#' Aggregate Data
#' 2024-09-26

#' Combine data from NASS and Data Warehouse, other sources, into one file


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr
)

# Load datasets
dat <- read_all_rds('5_objects/metrics/')  
get_str(dat)

# Load metadata
meta <- read_all_rds('5_objects/metadata/')
get_str(meta)



# Aggregate Metrics --------------------------------------------------------


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



# Metadata ----------------------------------------------------------------


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

# If there is no axis name, make it the variable_name
meta_agg <- meta_agg %>% 
  mutate(axis_name = ifelse(is.na(axis_name), variable_name, axis_name))



# Check and save ----------------------------------------------------------


# Check to make sure we have the same number of metrics and metas
try(check_n_records(agg, meta_agg, 'Aggregation'))



# CSV Files ---------------------------------------------------------------


# Save metrics and metadata as a zipped csv file for easy transport
paths <- c('6_outputs/metrics.csv', '6_outputs/metadata.csv')
write_csv(agg, paths[1])
write_csv(meta_agg, paths[2])

# Zip csv files together, remove original csvs
zip(
  zipfile = '6_outputs/metrics_and_metadata.zip', 
  files = c(paths)
)
file.remove(paths)



# End ---------------------------------------------------------------------


clear_data(gc = TRUE)
cat('\n*Aggregation complete*')