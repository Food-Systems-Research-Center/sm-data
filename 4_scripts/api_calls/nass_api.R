#' Pull NASS Data
#' 2025-07-02 update

# Pulling data from USDA NASS data through NASS API. Doing two separate calls,
# one for the census and one for the survey (yields). 


# Info on NASS QuickStat API:
# https://quickstats.nass.usda.gov/api/



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  purrr,
  readr,
  stringr,
  rnassqs
)

nass_key <- Sys.getenv('NASS_API_KEY')

# Define fips - have to split back into 2 digit state and 3 digit county to 
# query with NASS
states_only <- fips_key$fips[str_length(fips_key$fips) == 2]
all_counties <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  pull(fips)

# Use these two vectors to query. Goes through counties then states
county_fips <- c(str_sub(all_counties, start = 3), rep('', length(states_only)))
state_fips <- c(str_sub(all_counties, end = 2), states_only)
stopifnot(length(county_fips) == length(state_fips))



# Test rnassqs ------------------------------------------------------------


# Authorize API key
nassqs_auth(nass_key)

# Test run
out <- nassqs(
  commodity_desc = 'corn',
  year__GE = 2020,
  state_alpha = 'VT'
)
get_str(out)

# Test with relevant variable
out <- nassqs(
  short_desc = 'LABOR, HIRED - EXPENSE, MEASURED IN $',
  year__GE = 2020,
  state_alpha = 'VT'
)
get_str(out)

# Across two states, checking record counts
params <- list(
  short_desc = 'LABOR, HIRED - EXPENSE, MEASURED IN $',
  year__GE = 2020,
  state_alpha = c('VT', 'NH')
)
records <- nassqs_record_count(params)
stopifnot('Limit to 50,000 records per query' = records[[1]] < 50000)
out <- nassqs(params)
get_str(out)

# Across multiple counties (using 3 and 2 digit fips)
params <- list(
  # short_desc = 'LABOR, HIRED - EXPENSE, MEASURED IN $',
  short_desc = 'CHEMICAL TOTALS - EXPENSE, MEASURED IN $',
  year__GE = 2020,
  domain_desc = 'TOTAL',
  county_code = county_fips[1:3],
  state_fips_code = state_fips[1:3]
)
out <- nassqs(params)
get_str(out)
# Note that we need to specify the domain_desc or else we get many different
# breakdowns

# Multiple counties and multiple years
params <- list(
  source_desc = 'CENSUS',
  short_desc = 'LABOR, HIRED - EXPENSE, MEASURED IN $',
  domain_desc = 'TOTAL',
  county_code = county_fips[1:3],
  state_fips_code = state_fips[1:3]
)
years <- seq(2002, 2022, 5)
out <- map(years, \(yr) {
  param_set <- params
  param_set[['year']] <- yr
  nassqs(param_set)
}) %>% 
  list_rbind()
get_str(out)

# Add multiple vars at once
params <- list(
  source_desc = 'CENSUS',
  short_desc = c(
    'LABOR, HIRED - EXPENSE, MEASURED IN $',
    'CHEMICAL TOTALS - EXPENSE, MEASURED IN $'
  ),
  domain_desc = 'TOTAL',
  county_code = county_fips[1:3],
  state_fips_code = state_fips[1:3]
)
years <- seq(2002, 2022, 5)
out <- map(years, \(yr) {
  cat(
    '\nDownloading year', yr, '(', which(years == yr), 'of', length(years), ')\n\n'
  )
  param_set <- params
  param_set[['year']] <- yr
  Sys.sleep(1)
  nassqs(param_set)
}) %>% 
  purrr::list_rbind()
get_str(out)
# So we can just use vectors of short_descs at once


# Thoughts
# Each variable and year = 226 records. So we can do 220 vars per year before
# hitting issues with query size at 50000. Then just loop through each year,
# maybe with a Sys.sleep() for good measure

# So we need one big ole DF that has the short_descs we want. While we're at it,
# it would help to have the variable names, dimension, index, indicator, and
# really all of the metadata handy. Exceptions being years, latest year, etc.

# Let's just pull it from the metadata we already have to begin with. And then
# add the short_desc names which are sprinkled throughout the script. This 
# sounds like kind of an alright plan

# meta <- readRDS('5_objects/metadata/nass_meta.RDS')
# meta <- select(meta, dimension: units)
# get_str(meta)

# Save this as temp just in case. And also to csv so we can do this manually?
# saveRDS(meta, 'temp/old_nass_meta.RDS')
# write_csv(meta, 'temp/old_nass_meta.csv')

# Now add short_desc to each one I guess



# API Calls ---------------------------------------------------------------


# Authorize key
nassqs_auth(nass_key)

# Load API parameters doc
nass_params <- read_csv('5_objects/api_parameters/nass_api_parameters.csv')
get_str(nass_params)



## Census Calls ------------------------------------------------------------


census_params <- nass_params %>%
  filter(
    source_desc == 'CENSUS',
    short_desc != 'derived' | is.na(short_desc)
  )
get_str(census_params)
# 86

# # Test run with only the new variables
# census_params <- nass_params %>%
#   filter(
#     source_desc == 'CENSUS',
#     short_desc != 'derived' | is.na(short_desc),
#     note == 'new'
#   )
# get_str(census_params)
# # 13

# Set parameters
params <- list(
  source_desc = 'CENSUS',
  short_desc = census_params$short_desc,
  domain_desc = 'TOTAL',
  agg_level_desc = c('COUNTY', 'STATE'),
  state_fips_code = states_only[1:9]
)
years <- seq(2002, 2022, 5)
census_out <- map(years, \(yr) {
  cat(
    '\nDownloading year ', yr, ' (', which(years == yr), ' of ', length(years), ')\n\n',
    sep = ''
  )
  param_set <- params
  param_set[['year']] <- yr
  Sys.sleep(1)
  nassqs(param_set)
}) %>% 
  purrr::list_rbind()
get_str(census_out)

# Save this to API outs
saveRDS(census_out, '5_objects/api_outs/neast_nass_census_2002_2022.rds')



## Survey Calls ------------------------------------------------------------


# Don't need preset parameters, just taking all yield variables
params <- list(
  source_desc = 'SURVEY',
  domain_desc = 'TOTAL',
  statisticcat_desc = 'YIELD',
  agg_level_desc = c('COUNTY', 'STATE'),
  state_fips_code = states_only[1:9]
)
years <- seq(2002, 2022, 5)
survey_out <- map(years, \(yr) {
  cat(
    '\nDownloading year ', yr, ' (', which(years == yr), ' of ', length(years), ')\n\n',
    sep = ''
  )
  param_set <- params
  param_set[['year']] <- yr
  Sys.sleep(3)
  nassqs(param_set)
}) %>% 
  purrr::list_rbind()
get_str(survey_out)

# Save this to API outs
saveRDS(survey_out, '5_objects/api_outs/neast_nass_survey_2002_2022.rds')



# # Northeast Counties ------------------------------------------------------
# 
# 
# # NOTE: everything below is old, hanging on to it for now just in case
# # All Northeast counties, 2002 through 2022 by 5 years
# 
# # This will be all counties, all sectors, 2022
# base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'
# 
# # Other Parameters
# source_desc <- 'CENSUS'
# years <- seq(2007, 2022, 5)
# # freq_desc <- 'annual'
# 
# # For each year, pull data for all counties.
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
# 
# get_size(out)
# get_str(out)
# # 112MB for 4 years
# # Looks like we can avoid the 50k limit if they are separate calls.
# # No word on a request limit over time though.
# 
# # Save this for posterity
# # saveRDS(out, '5_objects/api_outs/nass_census_counties_2007-2022.rds')
# 
# # Process and save it
# get_str(out)
# counties_dat <- out %>% 
#   purrr::flatten() %>% 
#   bind_rows()
# get_str(counties_dat)
# 
# # Save into clean folder
# # saveRDS(counties_dat, '1_raw/nass/nass_census_counties_2007-2022.rds')
# 
# 
# 
# # All States --------------------------------------------------------------
# 
# 
# # In addition to all NE counties, we want ALL states and DC (51)
# 
# # Base request
# base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'
# 
# # Other Parameters
# source_desc <- 'CENSUS'
# years <- 2017
# state_fips <- unique(state_key$state_code)
# agg_level_desc <- 'STATE'
# # 
# # # For each year, pull data from each state
# # states_out <- map(years, \(year) {
# #   map(state_fips, \(state) {
# # 
# #     tryCatch({
# #       url <- glue(
# #         base_get,
# #         '?key={api_key}',
# #         '&state_fips_code={state}',
# #         '&agg_level_desc={agg_level_desc}',
# #         '&year={year}',
# #         '&source_desc={source_desc}',
# #         '&domain_desc=TOTAL'
# #       )
# #       print(url)
# #       GET(url) %>%
# #           content(as = 'text') %>%
# #           fromJSON() %>%
# #           .$data
# #     },
# #     error = function(e) {
# #       message('Call failed')
# #       print(e)
# #     })
# #       
# #   }) %>%
# #     setNames(c(paste0('state', state_fips))) %>%
# #     discard(is.null)
# # }) %>%
# #   setNames(c(paste0('year_', years)))
# # 
# # get_size(states_out)
# # get_str(states_out)
# 
# 
# ###
# # only_2017 <- states_out['year_2017']
# # get_str(only_2007)
# # get_str(only_2012)
# # get_str(only_2017)
# # get_str(only_2022)
# 
# # combine
# states_out <- c(only_2007, only_2012, only_2017, only_2022)
# states_dat <- states_out %>% 
#   purrr::flatten() %>% 
#   bind_rows()
# get_str(states_dat)
# 
# 
# 
# # Combine State and County ------------------------------------------------
# 
# 
# # Combining NE counties and all 51 states
# all_dat <- bind_rows(states_dat, counties_dat)
# get_str(all_dat)
# all_dat %>% 
#   select(state_fips_code, county_code) %>% 
#   unique %>% 
#   head(500)
# 
# # Save this as our real dataset
# saveRDS(all_dat, '1_raw/nass/ne_counties_all_states_2007-2022.rds')
# 
# 
# 
# # Yield Data - Survey -----------------------------------------------------
# 
# 
# # Pulling from 2022 survey to test out yield data. Just doing one year for now.
# 
# 
# # Base request
# base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'
# 
# # Other Parameters
# source_desc <- 'SURVEY'
# years <- 2022
# state_fips <- unique(state_key$state_code)
# agg_level_desc <- 'STATE'
# 
# 
# ## All states for one year
# out <- map(state_fips, \(state) {
#   tryCatch({
#     url <- glue(
#       base_get,
#       '?key={api_key}',
#       '&state_fips_code={state}',
#       '&agg_level_desc={agg_level_desc}',
#       '&year={years}',
#       '&source_desc={source_desc}',
#       '&domain_desc=TOTAL'
#     )
#     print(url)
#     GET(url) %>%
#       content(as = 'text') %>%
#       fromJSON() %>%
#       .$data
#   },
#   error = function(e) {
#     message('Call failed')
#     print(e)
#   }) 
# }) %>%
#   setNames(c(paste0('state', state_fips))) %>%
#   discard(is.null)
# 
# # Check it
# get_str(out)
# 
# # Combine it
# yield_dat <- out %>% 
#   bind_rows() %>% 
#   filter(
#     domain_desc == 'TOTAL',
#     statisticcat_desc == 'YIELD'
#   )
# 
# vars <- unique(yield_dat$short_desc)
# 
# # Check what is available for NE states
# ne_vars <- yield_dat %>% 
#   filter(state_alpha %in% c('VT', 'NH', 'ME', 'MA', 'CT', 'DE')) %>% 
#   pull(short_desc) %>% 
#   unique()
# # 26 vars represented in New England
# 
# # Save combined yield data to be wrangled in NASS compilation script
# saveRDS(out, '1_raw/nass/nass_survey_2022.rds')
# 
# 
# 
# # Water -------------------------------------------------------------------
# 
# 
# # Check out NASS water vars: economics > irrigation > pumps / water / wells
# # It apparently was surveyed in 2013, 2018, 2023, and only at state level
# 
# base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'
# 
# # Other Parameters
# source_desc <- 'CENSUS'
# sector_desc <- 'ECONOMICS'
# years <- seq(2013, 2023, 5)
# years <- 2023
# freq_desc <- 'annual'
# # all_state_fips <- unique(state_key$state_code)[1:25]
# all_state_fips <- unique(state_key$state_code)[26:length(unique(state_key$state_code))]
# # commodity_desc = c('WELLS', 'WATER')
# commodity_desc = 'WATER'
#  
# # water_vars <- c(paste(
# #   'WATER, IRRIGATION, RECLAIMED - WATER APPLIED, MEASURED IN ACRE FEET',
# #   'WATER, IRRIGATION, RECLAIMED, IN THE OPEN - ACRES IRRIGATED',
# #   'WATER, IRRIGATION, RECLAIMED, IN THE OPEN - OPERATIONS WITH AREA IRRIGATED',
# #   
# #   'WATER, IRRIGATION, SOURCE = OFF FARM - EXPENSE, MEASURED IN $',
# #   'WATER, IRRIGATION, SOURCE = OFF FARM - EXPENSE, MEASURED IN $ / ACRE FOOT',
# #   'WATER, IRRIGATION, SOURCE = OFF FARM - EXPENSE, MEASURED IN $ / ACRE IRRIGATED',
# #   'WATER, IRRIGATION, SOURCE = OFF FARM - OPERATIONS WITH EXPENSE',
# #   collapse = ','
# # ))
# 
# # For each year, pull data for all counties.
# out <- map(years, \(year) {
#   map(all_state_fips, \(state) {
#       
#       tryCatch({
#         url <- glue(
#           base_get,
#           '?key={api_key}',
#           '&state_fips_code={state}',
#           '&year={year}',
#           '&source_desc={source_desc}',
#           '&sector_desc={sector_desc}',
#           '&commodity_desc={commodity_desc}',
#           '&domain_desc=TOTAL'
#         )
#     
#         print(url)
#     
#         GET(url) %>%
#             content(as = 'text') %>%
#             fromJSON() %>%
#             .$data
#       }, error = function (e) {
#         message('Call failed')
#         print(e)
#       })
# 
#   })
# })
# 
# get_str(out)
# out1 <- out
# out2 <- out
# out3 <- out
# out4 <- out
# 
# water_dfs <- map(list(out1, out2, out3, out4), ~ {
#   map(.x, \(y) {
#     y %>% 
#       discard(is.null) %>% 
#       keep(is.data.frame) %>% 
#       .[[1]]
#   })
# })
# get_str(water_dfs, 3)
# 
# water <- water_dfs %>% 
#   purrr::flatten() %>% 
#   bind_rows()
# get_str(water)
# 
# water$short_desc
# 
# water <- water %>% 
#   rename(
#     fips = state_fips_code,
#     cv_percent = 'CV (%)',
#     value = Value
#   )
# get_str(water)
# saveRDS(water, '1_raw/nass/nass_states_irrigation_2013-2023_5yr.rds')
# 
