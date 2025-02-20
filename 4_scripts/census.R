# Census API
# 2024-10-02

# Variables list: https://api.census.gov/data/2023/acs/acs1/variables.html


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  readr,
  tibble,
  janitor,
  tidyr,
  stringr
)

# Load Census API Key
census_key <- Sys.getenv('ARMS_API_KEY')

# Function to pull from Census API, and filter by fips
source('3_functions/api/get_census_data.R')
source('3_functions/pipeline_utilities.R')
source('3_functions/metadata_utilities.R')

# county fips for New England (differences for CT restructuring)
fips_key <- readRDS('5_objects/fips_key.rds')
state_codes <- readRDS('5_objects/state_key.rds')

# lists of results
results <- list()
metas <- list()



# ACS 5-year --------------------------------------------------------------
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



## Pull County Data --------------------------------------------------------


# Set parameters
years <- c(2012:2022)
vars <- paste0(variables, collapse = ',')
states <- fips_key %>% 
  filter(str_length(fips) == 2 & fips != '00') %>% 
  pull(fips) %>% 
  paste0(collapse = ',')

# Map over list of years to gather data for each
counties_out <- map(years, \(year){
  get_census_data(
    survey_year = year,
    survey = 'acs/acs5',
    vars = vars,
    county = '*',
    state = states
  )
})
# Note that the first five years didn't come through. Only have 2015 to 2022
# So we only have 9?
get_str(counties_out)

# Save raw output
saveRDS(counties_out, '5_objects/api_outs/census_counties_2015_2022.rds')



## Pull State Data ---------------------------------------------------------


# Now all state data, but not counties.
# Set parameters
years <- seq(2015, 2022)
vars <- paste0(variables, collapse = ',')
states <- state_codes$state_code %>% 
  paste0(collapse = ',')

# Map over list of years to gather data for each
states_out <- map(years, \(year){
  get_census_data(
    survey_year = year,
    survey = 'acs/acs5',
    vars = vars,
    county = NULL,
    state = states
  )
})
# Note that the first five years didn't come through. Only have 2015 to 2022
# So we only have 9?
get_str(states_out)

# Save raw output
saveRDS(states_out, '5_objects/api_outs/census_states_2015_2022.rds')



# Wrangle -----------------------------------------------------------------


# Reload raw outputs from API calls
states_out <- readRDS('5_objects/api_outs/census_states_2015_2022.rds')
counties_out <- readRDS('5_objects/api_outs/census_counties_2015_2022.rds')

# Combine county and state data
dat <- reduce(c(counties_out, states_out), bind_rows) %>% 
  filter(is.na(status)) %>% 
  select(-c(status, query, GEO_ID, NAME))
get_str(dat)

# Combine years, convert to numeric, swap census names for our names
dat <- dat %>% 
  mutate(across(matches('^[A-Z][0-9]{5}_[0-9]{3}[A-Z]{1}$'), as.numeric)) %>% 
  setNames(c(recode(names(.), !!!crosswalk)))
get_str(dat)

# Calculations to get proportions for education, housing
dat <- dat %>% 
  mutate(
    edPercHSGED = ((edTotalHS + edTotalGED) / edTotal) * 100,
    edPercBS = (edTotalBS / edTotal) * 100,
    vacancyRate = (nHousingVacant / nHousingUnits) * 100
  ) %>% 
  select(-c(
    matches('edTotal'),
    state,
    county
   ))
get_str(dat)

# Now pivot longer to combine with other data
dat <- dat %>% 
  pivot_longer(
    cols = !c(fips, year),
    names_to = 'variable_name',
    values_to = 'value'
  ) %>% 
  mutate(value = as.character(value))
get_str(dat)

# Save it
results$acs5 <- dat



## Metadata ----------------------------------------------------------------


(vars <- get_vars(results$acs5))
# Have to redo this... []
# Before we pulled median earnings data from data warehouse
# Better to do it ourselves here.

# Save metadata
metas$acs5 <- tibble(
  dimension = c(
    rep("health", 11)
  ),
  index = c(
    rep('education', 2),
    rep('physical health', 9)
  ),
  indicator = c(
    rep('educational attainment', 2),
    rep('housing supply and quality', 9)
  ),
  metric = c(
    'Percentage with bachelor\'s degree',
    'Percentage with high school diploma, GED, or equivalent',
    'Median housing age',
    'Number of occupied housing units',
    'Number of total housing units',
    'Number of vacant housing units',
    'Median rent',
    'Median rent, 1br',
    'Median rent, 4br',
    'Median rent as a percentage of income',
    'Rental vacancy rate'
  ),
  variable_name = c(
    vars
  ),
  definition = c(
    'Percentage of population age 25 or older with bachelor\'s degree',
    'Percentage of population age 25 or older with high school diploma, GED, or equivalent',
    'Median year in which housing unit was constructed',
    'Number of occupied housing units',
    'Number of total housing units',
    'Number of vacant housing units',
    'Median gross rent, aggregate',
    'Median gross rent for a 1-bedroom apartment',
    'Median gross rent for a 4-bedroom apartment',
    'Median gross rent as a percentage of household income over the last 12 months',
    'Vacancy rate, rentals'
  ),
  axis_name = c(
    'Bachelor\'s degree (%)',
    'HS or GED (%)',
    'Median housing year',
    'Number of occupied units',
    'Number of housing units',
    'Number of vacant units',
    'Median rent',
    'Median rent 1BR',
    'Median rent 4BR',
    'Rent as % of income',
    'Vacancy rate'
  ),
  units = c(
    rep('percentage', 2),
    'year',
    rep('count', 3),
    rep('usd', 4),
    'percentage'
  ),
  scope = 'national',
  resolution = 'county, state',
  year = get_all_years(results$acs5),
  latest_year = get_max_year(results$acs5),
  updates = "5 years",
  source = 'U.S. Census Bureau, American Community Survey: 5-Year Estimates: Detailed Tables, 2022',
  url = 'https://www.census.gov/data/developers/data-sets/acs-5year.html',
  warehouse = FALSE
) %>% 
  add_citation(access_date = '2025-01-08')

  
get_str(metas$acs5)



# Gini Index --------------------------------------------------------------


gini_vars <- c('B19083_001E', "B19083_001M")

# Set parameters
years <- as.list(seq(2010, 2022, 1))
vars <- paste0(gini_vars, collapse = ',')
states <- fips_key %>% 
  filter(str_length(fips) == 2 & fips != '00') %>% 
  pull(fips) %>% 
  paste0(collapse = ',')

# Map over list of years to gather data for each
counties_out <- map(years, \(year){
  get_census_data(
    survey_year = year,
    survey = 'acs/acs5',
    vars = vars,
    county = '*',
    state = states
  )
})
get_str(counties_out)


# Set parameters for state data
years <- as.list(seq(2010, 2022, 1))
vars <- paste0(gini_vars, collapse = ',')
states <- state_codes$state_code %>% 
  paste0(collapse = ',')

# Map over list of years to gather data for each
states_out <- map(years, \(year){
  get_census_data(
    survey_year = year,
    survey = 'acs/acs5',
    vars = vars,
    county = NULL,
    state = states
  )
})
get_str(states_out)

# Clean and combine
results$gini <- reduce(c(states_out, counties_out), bind_rows) %>% 
  select(
    fips,
    year,
    value = B19083_001E,
    margin = B19083_001M
  ) %>% 
  mutate(variable_name = 'gini')
get_str(results$gini)


metas$gini <- tibble(
  dimension = 'economics',
  index = 'community economy',
  indicator = 'wealth/income distribution',
  metric = 'Gini Index',
  variable_name = 'gini',
  definition = 'Gini Index of income inequality. 0 is perfect inequality, while 1 is perfect inequality',
  axis_name = 'Gini index',
  units = 'index',
  scope = 'national',
  resolution = 'county, state',
  year = get_all_years(results$gini),
  latest_year = get_max_year(results$gini),
  updates = "5 years",
  source = 'U.S. Census Bureau, American Community Survey: 5-Year Estimates: Detailed Tables, 2022.',
  url = 'https://www.census.gov/data/developers/data-sets/acs-5year.html',
  warehouse = FALSE
) %>% 
  add_citation('2025-01-08')

get_str(metas$gini)



# Wages -------------------------------------------------------------------




# Save and Clear ----------------------------------------------------------


## Clean and bind results
get_str(results)
map(results, get_str)

# Make value and year numeric across all datasets, then combine
result <- map(results, \(df) {
  df %>% 
    mutate(
      value = as.numeric(value),
      year = as.numeric(year)
    )
}) %>% bind_rows()
get_str(result)


## Bind metadata
get_str(metas)
map(metas, get_str)
meta <- bind_rows(metas)
get_str(meta)


## Check to make sure we have the same number of metrics and metas
check_n_records(result, meta, 'Census')

# Save them
saveRDS(result, '5_objects/metrics/census.RDS')
saveRDS(meta, '5_objects/metadata/census_meta.RDS')

clear_data()
gc()
