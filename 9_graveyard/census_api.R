# Census API
# 2025-06-30


# Description -------------------------------------------------------------

# Using SMdata::call_census_api() to pull data from American Community Survey
# 5 year. 

# Note: Should lump in gini into same calls next time we run this



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  purrr,
  stringr,
  janitor,
  censusapi,
  glue
)

census_key <- Sys.getenv("CENSUS_API_KEY")



# Rework ------------------------------------------------------------------
## Testing -----------------------------------------------------------------

# Try out censusapi
apis <- listCensusApis()
get_str(apis)

# Specifics about acs5
acs_apis <- listCensusApis(name = 'acs/acs5', vintage = 2022)

# Metadata for acs5
meta <- listCensusMetadata('acs/acs5', vintage = 2022)
get_str(meta)



## Prep --------------------------------------------------------------------


# Get vector of northeast states
state_codes <- fips_key %>% 
  filter(str_length(fips) == 2, fips != '00') %>% 
  pull(fips)

# Years
years <- seq(2008, 2023, 5)

# Variables
vars <- list(
  # Population
  'population' = 'B01003_001E',
  
  # Education
  'edTotal' = 'B15003_001E',
  'edTotalHS' = 'B15003_017E',
  'edTotalGED' = 'B15003_018E',
  'edTotalBS' = 'B15003_022E',
  
  # Housing
  'nHousingUnits' = 'B25001_001E',
  'nHousingOccupied' = 'B25002_002E',
  'nHousingVacant' = 'B25002_003E',
  
  # Rent by bedrooms
  'rentMedian1BR' = 'B25031_003E',
  'rentMedian4BR' = 'B25031_006E',
  
  # Housing age
  'medianHousingYear' = 'B25035_001E',
   
  # More rent
  'rentMedian' = 'B25064_001E',
  'rentMedianPercHH' = 'B25071_001E',
  
  # Wages in FFF
  'medianFemaleEarningsFFF' = 'B24022_067E',
  'medianMaleEarningsFFF' = 'B24022_031E',
  'medianFemaleEarningsFPS' = 'B24022_060E',
  'medianMaleEarningsFPS' = 'B24022_024E',
  
  # Gini coefficient
  'gini' = 'B19083_001E'
)




## Small Tests -------------------------------------------------------------


# Map over states, get state data
# one year only
out <- map(state_codes, \(state_code) {
  Sys.sleep(2)
  getCensus(
    name = "acs/acs5",
    vintage = 2023,
    key = census_key,
    vars = vars,
    region = paste0("state:", state_code)
  )
}) %>% 
  bind_rows()
get_str(out)
# State data checks out - adds up to sum of counties approximately
# looks like it will be easiest to just run them both separately and combine


# Now map over multiple years (few states to test)
years <- c(2018, 2023)
county_out <- map(state_codes[1:2], \(state_code) {
  map(years, \(yr) {
    Sys.sleep(2)
    getCensus(
      name = "acs/acs5",
      key = census_key,
      vintage = yr,
      vars = vars,
      region = "county:*",
      regionin = paste0("state:", state_code)
    ) %>% 
      mutate(year = yr)
  }) %>%
    bind_rows()
}) %>% 
  bind_rows()
get_str(county_out)



## Function ----------------------------------------------------------------


# Having some trouble getting package function to work - leaving this here
# for now just in case.

# census_api <- function(state_codes,
#                        years,
#                        vars,
#                        census_key,
#                        region = c('state', 'county'),
#                        sleep_time = 1,
#                        survey_name = 'acs/acs5') {
#   map(state_codes, \(state_code) {
#     
#     cat(glue("\n\nStarting state: {state_code} ({which(state_codes == state_code)} of {length(state_codes)})\n"))
#     
#     # Set region based on state or county data
#     if (region == 'state') {
#       region_var <- paste0('state:', state_code)
#       regionin_var <- NULL
#     }
#     else if (region == 'county') {
#       region_var <- "county:*"
#       regionin_var <- paste0("state:", state_code)
#     }
#     
#     map(years, \(yr) {
#       cat('\nStarting year', yr, '\n')
#       Sys.sleep(sleep_time)
#       vars_out <- map(vars, \(var) {
#         tryCatch(
#           {
#             getCensus(
#               name = survey_name,
#               vintage = yr,
#               key = census_key,
#               vars = var,
#               region = region_var,
#               regionin = regionin_var
#             ) %>% 
#               mutate(year = yr)  
#           },
#           error = function(e) {
#             message(glue("Error for state {state_code}, year {yr}: {e$message}"))
#             tibble()
#           }
#         )
#       }) %>% 
#         purrr::keep(~ is.data.frame(.x) && nrow(.x) > 0)
#       
#       if (length(vars_out) > 1) {
#         purrr::reduce(vars_out, full_join)
#       } else if (length(vars_out) == 1) {
#         vars_out[[1]]
#       } else {
#         NULL
#       }
#       
#     }) %>% 
#       purrr::keep(~ !is.null(.x)) %>% 
#       bind_rows()
#   }) %>% 
#     purrr::keep(~ !is.null(.x)) %>% 
#     bind_rows()
# }



## Pull with function ------------------------------------------------------


# State data
states_out <- call_census_api(
  state_codes = state_codes,
  years = years,
  vars = vars,
  census_key = census_key,
  region = 'state'
)
get_str(states_out)  
# saveRDS(states_out, 'temp/states_out.rds')

# County data
county_out <- call_census_api(
  state_codes = state_codes,
  years = years,
  vars = vars,
  census_key = census_key,
  region = 'county'
)
get_str(county_out)  
# saveRDS(county_out, 'temp/county_out.rds')

# Combine and save
out <- bind_rows(states_out, county_out)
get_str(out)
saveRDS(out, '5_objects/api_outs/acs5_neast_counties_states_2008_2023.rds')



# ACS 5yr -----------------------------------------------------------------

## Variables ---------------------------------------------------------------


#' NOTE
#' _000E is the estimate
#' _000M is the margin of error
#' _000EA and _000MA are annotations for both. Not always there though

# All relevant variables for American Community Survey data
variables <- list(
  # Population
  'population' = 'B01003_001E',
  
  # Education
  'edTotal' = 'B15003_001E',
  'edTotalHS' = 'B15003_017E',
  'edTotalGED' = 'B15003_018E',
  'edTotalBS' = 'B15003_022E',
  
  # Housing
  'nHousingUnits' = 'B25001_001E',
  'nHousingOccupied' = 'B25002_002E',
  'nHousingVacant' = 'B25002_003E',
  
  # Rent by bedrooms
  'rentMedian1BR' = 'B25031_003E',
  'rentMedian4BR' = 'B25031_006E',
  
  # Housing age
  'medianHousingYear' = 'B25035_001E',
   
  # More rent
  'rentMedian' = 'B25064_001E',
  'rentMedianPercHH' = 'B25071_001E',
  
  # Wages in FFF
  'medianFemaleEarningsFFF' = 'B24022_067E',
  'medianMaleEarningsFFF' = 'B24022_031E',
  'medianFemaleEarningsFPS' = 'B24022_060E',
  'medianMaleEarningsFPS' = 'B24022_024E'
)

# B15003_001E educational attainment total
# B15003_017E high school diploma
# B15003_018E ged or equivalent
# B15003_022E bachelors

# B25001_001E is total housing units
# B25002_001E is total occupancy status (used to calculate vacancy)
# B25002_002E is estimated occupied
# B25002_003E is estimated vacant

# B25031_003E median gross rent for 1-bedroom
# B25031_006E median gross rent for 4-bedroom
# B25034_001E through B25045_011E year structure built by bin
#   This would be a nice way to get an age demographic on housing
# B25035_001E is median age structure built

# B25064_001E median gross rent
# B25071_001E median gross rent as percentage of household income in last year


# Also turn it into a DF
crosswalk <- setNames(names(variables), variables)
saveRDS(crosswalk, '5_objects/census_acs5_crosswalk.rds')



## Pull County Data --------------------------------------------------------


# # Set parameters
# years <- c(2012:2022)
# vars <- paste0(variables, collapse = ',')
# states <- fips_key %>% 
#   filter(str_length(fips) == 2 & fips != '00') %>% 
#   pull(fips) %>% 
#   paste0(collapse = ',')
# 
# # Map over list of years to gather data for each
# counties_out <- map(years, \(year){
#   get_census_data(
#     survey_year = year,
#     survey = 'acs/acs5',
#     vars = vars,
#     county = '*',
#     state = states
#   )
# })
# # Note that the first five years didn't come through. Only have 2015 to 2022
# # So we only have 9?
# get_str(counties_out)
# 
# # Save raw output
# saveRDS(counties_out, '5_objects/api_outs/census_counties_2015_2022.rds')



## Pull State Data ---------------------------------------------------------


# # Now all state data, but not counties.
# # Set parameters
# years <- seq(2015, 2022)
# vars <- paste0(variables, collapse = ',')
# states <- state_codes$state_code %>% 
#   paste0(collapse = ',')
# 
# # Map over list of years to gather data for each
# states_out <- map(years, \(year){
#   get_census_data(
#     survey_year = year,
#     survey = 'acs/acs5',
#     vars = vars,
#     county = NULL,
#     state = states
#   )
# })
# # Note that the first five years didn't come through. Only have 2015 to 2022
# # So we only have 9?
# get_str(states_out)
# 
# # Save raw output
# saveRDS(states_out, '5_objects/api_outs/census_states_2015_2022.rds')



# Gini --------------------------------------------------------------------


# Note: Should be able to lump these into to last call next time we want to 
# run this


# gini_vars <- c('B19083_001E', "B19083_001M")
# 
# # Set parameters
# years <- as.list(seq(2010, 2022, 1))
# vars <- paste0(gini_vars, collapse = ',')
# states <- fips_key %>% 
#   filter(str_length(fips) == 2 & fips != '00') %>% 
#   pull(fips) %>% 
#   paste0(collapse = ',')
# 
# # Map over list of years to gather data for each
# counties_out <- map(years, \(year){
#   get_census_data(
#     survey_year = year,
#     survey = 'acs/acs5',
#     vars = vars,
#     county = '*',
#     state = states
#   )
# })
# get_str(counties_out)
# 
# 
# # Set parameters for state data
# years <- as.list(seq(2010, 2022, 1))
# vars <- paste0(gini_vars, collapse = ',')
# states <- state_codes$state_code %>% 
#   paste0(collapse = ',')
# 
# # Map over list of years to gather data for each
# states_out <- map(years, \(year){
#   get_census_data(
#     survey_year = year,
#     survey = 'acs/acs5',
#     vars = vars,
#     county = NULL,
#     state = states
#   )
# })
# get_str(states_out)
# 
# # Save out files
# saveRDS(counties_out, '5_objects/api_outs/gini_census_acs5_counties_2010_2022.rds')
# saveRDS(states_out, '5_objects/api_outs/gini_census_acs5_states_2010_2022.rds')
