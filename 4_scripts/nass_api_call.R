#' Pull NASS Data
#' 024-09-09

# Pulling data from USDA NASS for SM secondary data


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  readr,
  stringr
)

key <- Sys.getenv('NASS_API_KEY')
source('3_functions/api_functions.R')
fips_key <- readRDS('5_objects/fips_key.rds')

# Define fips - have to split back into 2 digit state and 3 digit county
full_fips <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  pull(fips)
county_fips <- str_sub(full_fips, start = 3)
state_fips <- str_sub(full_fips, end = 2)



# Everything --------------------------------------------------------------


# All New England counties, 2007 through 2022 by 5 years

# This will be all counties, all sectors, 2022
base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'

# Other Parameters
source_desc <- 'CENSUS'
years <- seq(2007, 2022, 5)
freq_desc <- 'annual'

# For each year, pull data for all counties.
# out <- map(years, \(year) {
#   map2(county_fips, state_fips, \(county, state) {
#     
#     url <- glue(
#       base_get,
#       '?key={api_key}',
#       '&county_ansi={county}',
#       '&state_fips_code={state}',
#       '&year={year}',
#       '&source_desc={source_desc}',
#       '&domain_desc=TOTAL'
#     )
#     
#     print(url)
#     
#     GET(url) %>%
#         content(as = 'text') %>%
#         fromJSON() %>%
#         .$data
#     
#   }) %>% 
#     setNames(c(paste0('county_', state_fips, county_fips))) %>% 
#     discard(is.null)
# }) %>% 
#   setNames(c(paste0('year_', years)))

get_size(out)
get_str(out)
# 112MB for 4 years
# Looks like we can avoid the 50k limit if they are separate calls.
# No word on a request limit over time though.

# Save this for posterity
# saveRDS(out, '5_objects/api_outs/nass_census_counties_2007-2022.rds')

# Process and save it
get_str(out)
dat <- out %>% 
  flatten() %>% 
  bind_rows()
get_str(dat)

# Save into clean folder
saveRDS(dat, '1_raw/nass/nass_census_counties_2007-2022.rds')



# Water -------------------------------------------------------------------


# Check out NASS water vars: economics > irrigation > pumps / water / wells
# It apparently was surveyed in 2013, 2018, 2023, and only at state level

base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'

# Other Parameters
source_desc <- 'CENSUS'
sector_desc <- 'ECONOMICS'
years <- seq(2013, 2023, 5)
freq_desc <- 'annual'
unique_state_fips <- c(unique(state_fips))

# For each year, pull data for all counties.
# out <- map(years, \(year) {
#   map(unique_state_fips, \(state) {
# 
#     url <- glue(
#       base_get,
#       '?key={api_key}',
#       '&state_fips_code={state}',
#       '&year={year}',
#       '&source_desc={source_desc}',
#       '&sector_desc={sector_desc}',
#       '&domain_desc=TOTAL'
#     )
# 
#     print(url)
# 
#     GET(url) %>%
#         content(as = 'text') %>%
#         fromJSON() %>%
#         .$data
# 
#   }) %>%
#     setNames(c(unique_state_fips)) %>%
#     discard(is.null)
# }) %>%
#   setNames(c(paste0('year_', years)))

get_str(out)

water <- out %>% 
  flatten() %>% 
  bind_rows()
get_str(water)

water <- water %>% 
  rename(
    fips = state_fips_code,
    cv_percent = 'CV (%)',
    value = Value
  )
get_str(water)
saveRDS(water, '1_raw/nass/nass_states_irrigation_2013-2023_5yr.rds')

