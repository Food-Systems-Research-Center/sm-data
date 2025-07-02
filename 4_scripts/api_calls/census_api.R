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
  censusapi
)



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
  'medianMaleEarningsFPS' = 'B24022_024E'
)



## Try It ------------------------------------------------------------------


# Map over states, get data from counties in each
# one year only
out <- map(state_codes, \(state_code) {
  cat(
    '\nDownloading year ', yr, ' (', which(years == yr), ' of ', length(years), ')\n\n',
    sep = ''
  )
  Sys.sleep(2)
  getCensus(
    name = "acs/acs5",
    vintage = 2023,
    vars = vars,
    region = "county:*",
    regionin = paste0("state:", state_code)
  )
})
get_str(out)

# Now map over multiple years (few states to test)
years <- c(2018, 2023)
out <- map(state_codes[1:2], \(state_code) {
  map(years, \(yr) {
    Sys.sleep(2)
    getCensus(
      name = "acs/acs5",
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
get_str(out)
# [] this all works - ready to do it all at once
# remember to grab gini from below while we're at it
# Consider combining with the state data below too... ideally one call
# also could clean up our fucking variable list down there man



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
