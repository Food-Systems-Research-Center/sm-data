#' Explore
#' 2024-09-20


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr,
  mapview,
  leaflet,
  viridisLite,
  RColorBrewer,
  stringr,
  sf,
  rlang,
  tidyr,
  ggplot2,
  plotly
)

source('3_functions/data_pipeline_functions.R')
source('3_functions/read_all_rds.R')

# Aggregated dataset by county
dat <- readRDS('2_clean/metrics.rds')

# Metadata
meta <- readRDS('2_clean/metadata.rds')

# Spatial objects and references
counties <- read_all_rds('2_clean/spatial/', pattern = '^ne_counties_')
fips_key <- readRDS('5_objects/fips_key.rds')
state_key <- readRDS('5_objects/state_key.rds')



# Check Metrics -----------------------------------------------------------

get_str(dat)
get_str(meta)

env <- meta %>% 
  filter(dimension == 'environment')
env$metric %>%
  sort

dat %>% 
  filter(str_detect(variable_name, '^lulc'))
         