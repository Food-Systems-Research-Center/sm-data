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

# Check variables
(vars <- dat$variable_name %>% unique)
length(vars)



# Check meta --------------------------------------------------------------


get_str(meta)
dim(meta)
# 84



# Check diference ---------------------------------------------------------


setdiff(
  meta$variable_name,
  vars
  # unique(dat$variable_name)
)

meta %>% 
  filter(dimension == 'health') %>% 
  select(metric, variable_name, year) %>% 
  print(n = 100)
# Issue is wi and snap stuff



# Test Colors -------------------------------------------------------------


brewer.pal.info
display.brewer.all()
brewer.pal(9, 'YlOrRd')

