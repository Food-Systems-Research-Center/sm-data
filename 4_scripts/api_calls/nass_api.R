#' Pull NASS Data
#' 2025-07-02 update

# Pulling data from USDA NASS data through NASS API. Doing two separate calls,
# one for the census and one for the survey (yields). 

# Info on NASS QuickStat API:
# https://quickstats.nass.usda.gov/api/


# Note: clean up the testing parts of script, turn census calls into a function
# to use on both survey and census



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

# Authorize nass api key
nass_key <- Sys.getenv('NASS_API_KEY')
nassqs_auth(nass_key)

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


# # Authorize API key
# nassqs_auth(nass_key)
# 
# # Test run
# out <- nassqs(
#   commodity_desc = 'corn',
#   year__GE = 2020,
#   state_alpha = 'VT'
# )
# get_str(out)
# 
# # Test with relevant variable
# out <- nassqs(
#   short_desc = 'LABOR, HIRED - EXPENSE, MEASURED IN $',
#   year__GE = 2020,
#   state_alpha = 'VT'
# )
# get_str(out)
# 
# # Across two states, checking record counts
# params <- list(
#   short_desc = 'LABOR, HIRED - EXPENSE, MEASURED IN $',
#   year__GE = 2020,
#   state_alpha = c('VT', 'NH')
# )
# records <- nassqs_record_count(params)
# stopifnot('Limit to 50,000 records per query' = records[[1]] < 50000)
# out <- nassqs(params)
# get_str(out)
# 
# # Across multiple counties (using 3 and 2 digit fips)
# params <- list(
#   # short_desc = 'LABOR, HIRED - EXPENSE, MEASURED IN $',
#   short_desc = 'CHEMICAL TOTALS - EXPENSE, MEASURED IN $',
#   year__GE = 2020,
#   domain_desc = 'TOTAL',
#   county_code = county_fips[1:3],
#   state_fips_code = state_fips[1:3]
# )
# out <- nassqs(params)
# get_str(out)
# # Note that we need to specify the domain_desc or else we get many different
# # breakdowns
# 
# # Multiple counties and multiple years
# params <- list(
#   source_desc = 'CENSUS',
#   short_desc = 'LABOR, HIRED - EXPENSE, MEASURED IN $',
#   domain_desc = 'TOTAL',
#   county_code = county_fips[1:3],
#   state_fips_code = state_fips[1:3]
# )
# years <- seq(2002, 2022, 5)
# out <- map(years, \(yr) {
#   param_set <- params
#   param_set[['year']] <- yr
#   nassqs(param_set)
# }) %>% 
#   list_rbind()
# get_str(out)
# 
# # Add multiple vars at once
# params <- list(
#   source_desc = 'CENSUS',
#   short_desc = c(
#     'LABOR, HIRED - EXPENSE, MEASURED IN $',
#     'CHEMICAL TOTALS - EXPENSE, MEASURED IN $'
#   ),
#   domain_desc = 'TOTAL',
#   county_code = county_fips[1:3],
#   state_fips_code = state_fips[1:3]
# )
# years <- seq(2002, 2022, 5)
# out <- map(years, \(yr) {
#   cat(
#     '\nDownloading year', yr, '(', which(years == yr), 'of', length(years), ')\n\n'
#   )
#   param_set <- params
#   param_set[['year']] <- yr
#   Sys.sleep(1)
#   nassqs(param_set)
# }) %>% 
#   purrr::list_rbind()
# get_str(out)
# # So we can just use vectors of short_descs at once


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

# Check if we got everything
setdiff(census_params$short_desc, census_out$short_desc)

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
  Sys.sleep(1)
  nassqs(param_set)
}) %>% 
  purrr::list_rbind()
get_str(survey_out)

# Save this to API outs
saveRDS(survey_out, '5_objects/api_outs/neast_nass_survey_2002_2022.rds')

clear_data(gc = TRUE)
