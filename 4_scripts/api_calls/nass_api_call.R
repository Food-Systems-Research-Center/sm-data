#' Pull NASS Data
#' 2025-05-26 update

#' Pulling data from USDA NASS data through NASS API. Note that county API calls
#' are currently hinky and require running them individually and stitching 
#' together manually. API calls are commented out until that is done.

#' To Do:
#'  Fix county API calls
#'  Change where API outs are being saved - swap it to 5_objects/api_outs
#'  Memoize everything



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

api_key <- Sys.getenv('NASS_API_KEY')
source('3_functions/api/api_functions.R')
fips_key <- readRDS('5_objects/fips_key.rds')
state_key <- readRDS('5_objects/state_key.rds')

# Define fips - have to split back into 2 digit state and 3 digit county
full_fips <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  pull(fips)
county_fips <- str_sub(full_fips, start = 3)
state_fips <- str_sub(full_fips, end = 2)



# NE Counties -------------------------------------------------------------


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
counties_dat <- out %>% 
  purrr::flatten() %>% 
  bind_rows()
get_str(counties_dat)

# Save into clean folder
# saveRDS(counties_dat, '1_raw/nass/nass_census_counties_2007-2022.rds')



# All States --------------------------------------------------------------


# In addition to all NE counties, we want ALL states and DC (51)

# Base request
base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'

# Other Parameters
source_desc <- 'CENSUS'
years <- 2017
state_fips <- unique(state_key$state_code)
agg_level_desc <- 'STATE'
# 
# # For each year, pull data from each state
# states_out <- map(years, \(year) {
#   map(state_fips, \(state) {
# 
#     tryCatch({
#       url <- glue(
#         base_get,
#         '?key={api_key}',
#         '&state_fips_code={state}',
#         '&agg_level_desc={agg_level_desc}',
#         '&year={year}',
#         '&source_desc={source_desc}',
#         '&domain_desc=TOTAL'
#       )
#       print(url)
#       GET(url) %>%
#           content(as = 'text') %>%
#           fromJSON() %>%
#           .$data
#     },
#     error = function(e) {
#       message('Call failed')
#       print(e)
#     })
#       
#   }) %>%
#     setNames(c(paste0('state', state_fips))) %>%
#     discard(is.null)
# }) %>%
#   setNames(c(paste0('year_', years)))
# 
# get_size(states_out)
# get_str(states_out)


###
# only_2017 <- states_out['year_2017']
# get_str(only_2007)
# get_str(only_2012)
# get_str(only_2017)
# get_str(only_2022)

# combine
states_out <- c(only_2007, only_2012, only_2017, only_2022)
states_dat <- states_out %>% 
  purrr::flatten() %>% 
  bind_rows()
get_str(states_dat)



# Combine State and County ------------------------------------------------


# Combining NE counties and all 51 states
all_dat <- bind_rows(states_dat, counties_dat)
get_str(all_dat)
all_dat %>% 
  select(state_fips_code, county_code) %>% 
  unique %>% 
  head(500)

# Save this as our real dataset
saveRDS(all_dat, '1_raw/nass/ne_counties_all_states_2007-2022.rds')



# Yield Data - Survey -----------------------------------------------------


# Pulling from 2022 survey to test out yield data. Just doing one year for now.


# Base request
base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'

# Other Parameters
source_desc <- 'SURVEY'
years <- 2022
state_fips <- unique(state_key$state_code)
agg_level_desc <- 'STATE'


## All states for one year
out <- map(state_fips, \(state) {
  tryCatch({
    url <- glue(
      base_get,
      '?key={api_key}',
      '&state_fips_code={state}',
      '&agg_level_desc={agg_level_desc}',
      '&year={years}',
      '&source_desc={source_desc}',
      '&domain_desc=TOTAL'
    )
    print(url)
    GET(url) %>%
      content(as = 'text') %>%
      fromJSON() %>%
      .$data
  },
  error = function(e) {
    message('Call failed')
    print(e)
  }) 
}) %>%
  setNames(c(paste0('state', state_fips))) %>%
  discard(is.null)

# Check it
get_str(out)

# Combine it
yield_dat <- out %>% 
  bind_rows() %>% 
  filter(
    domain_desc == 'TOTAL',
    statisticcat_desc == 'YIELD'
  )

vars <- unique(yield_dat$short_desc)

# Check what is available for NE states
ne_vars <- yield_dat %>% 
  filter(state_alpha %in% c('VT', 'NH', 'ME', 'MA', 'CT', 'DE')) %>% 
  pull(short_desc) %>% 
  unique()
# 26 vars represented in New England

# Save combined yield data to be wrangled in NASS compilation script
saveRDS(out, '1_raw/nass/nass_survey_2022.rds')



# Water -------------------------------------------------------------------


# Check out NASS water vars: economics > irrigation > pumps / water / wells
# It apparently was surveyed in 2013, 2018, 2023, and only at state level

base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'

# Other Parameters
source_desc <- 'CENSUS'
sector_desc <- 'ECONOMICS'
years <- seq(2013, 2023, 5)
years <- 2023
freq_desc <- 'annual'
# all_state_fips <- unique(state_key$state_code)[1:25]
all_state_fips <- unique(state_key$state_code)[26:length(unique(state_key$state_code))]
# commodity_desc = c('WELLS', 'WATER')
commodity_desc = 'WATER'
 
# water_vars <- c(paste(
#   'WATER, IRRIGATION, RECLAIMED - WATER APPLIED, MEASURED IN ACRE FEET',
#   'WATER, IRRIGATION, RECLAIMED, IN THE OPEN - ACRES IRRIGATED',
#   'WATER, IRRIGATION, RECLAIMED, IN THE OPEN - OPERATIONS WITH AREA IRRIGATED',
#   
#   'WATER, IRRIGATION, SOURCE = OFF FARM - EXPENSE, MEASURED IN $',
#   'WATER, IRRIGATION, SOURCE = OFF FARM - EXPENSE, MEASURED IN $ / ACRE FOOT',
#   'WATER, IRRIGATION, SOURCE = OFF FARM - EXPENSE, MEASURED IN $ / ACRE IRRIGATED',
#   'WATER, IRRIGATION, SOURCE = OFF FARM - OPERATIONS WITH EXPENSE',
#   collapse = ','
# ))

# For each year, pull data for all counties.
out <- map(years, \(year) {
  map(all_state_fips, \(state) {
      
      tryCatch({
        url <- glue(
          base_get,
          '?key={api_key}',
          '&state_fips_code={state}',
          '&year={year}',
          '&source_desc={source_desc}',
          '&sector_desc={sector_desc}',
          '&commodity_desc={commodity_desc}',
          '&domain_desc=TOTAL'
        )
    
        print(url)
    
        GET(url) %>%
            content(as = 'text') %>%
            fromJSON() %>%
            .$data
      }, error = function (e) {
        message('Call failed')
        print(e)
      })

  })
})

get_str(out)
out1 <- out
out2 <- out
out3 <- out
out4 <- out

water_dfs <- map(list(out1, out2, out3, out4), ~ {
  map(.x, \(y) {
    y %>% 
      discard(is.null) %>% 
      keep(is.data.frame) %>% 
      .[[1]]
  })
})
get_str(water_dfs, 3)

water <- water_dfs %>% 
  purrr::flatten() %>% 
  bind_rows()
get_str(water)

water$short_desc

water <- water %>% 
  rename(
    fips = state_fips_code,
    cv_percent = 'CV (%)',
    value = Value
  )
get_str(water)
saveRDS(water, '1_raw/nass/nass_states_irrigation_2013-2023_5yr.rds')

