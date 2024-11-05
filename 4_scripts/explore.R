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
# Includes NAICS vars...

# Check without NAICS
no_NAICS <- vars %>% 
  str_subset('NAICS', negate = TRUE)
length(no_NAICS)



# Check meta --------------------------------------------------------------


get_str(meta)
meta$variable_name %>% 
  length

meta$variable_name %>% 
  str_subset('Naics', negate = TRUE) %>% 
  length

meta$variable_name %>% 
  unique %>% 
  str_subset(regex('wage', ignore_case = TRUE))



# Check difference ---------------------------------------------------------


diff <- setdiff(
  meta$variable_name,
  vars
)
diff
# the lq and oty stuff are because in the variable names they are split into 
# NAICS codes.

# Check without those
diff %>% 
  str_subset('^oty|^lq', negate = TRUE)

# Explore these
meta$variable_name %>% sort
vars %>% sort
# Okay these are all fine - they are just split out into NAICS codes too



# Rando -------------------------------------------------------------------

# Check out what NAICS data looks like - is it raw or percent
get_str(dat)
dat %>% 
  filter(str_detect(variable_name, 'EmpLvl'))
# These are raw numbers. Percentages would be nice, but we need civilian 
# labor force


# Test Colors -------------------------------------------------------------


brewer.pal.info
display.brewer.all()
brewer.pal(9, 'YlOrRd')

