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
# census_key <- Sys.getenv('ARMS_API_KEY')

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
  # Education
  'edTotal' = 'B15003_001E',
  'edTotalHS' = 'B15003_017E',
  'edTotalGED' = 'B15003_018E',
  'edTotalBS' = 'B15003_022E',
  
  # Housing
  'nHousingUnits' = 'B25001_001E',
  # 'total_occupancy_status' = 'B25002_001E',
  'nHousingOccupied' = 'B25002_002E',
  'nHousingVacant' = 'B25002_003E',
  
  # Rent by bedrooms
  'rentMedian1BR' = 'B25031_003E',
  'rentMedian4BR' = 'B25031_006E',
  
  # Housing age
  'medianHousingYear' = 'B25035_001E',
   
  # More rent
  'rentMedian' = 'B25064_001E',
  'rentMedianPercHH' = 'B25071_001E'
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


## Pull Data ---------------------------------------------------------------


#' Note that only 2017 and 2022 are working for some reason. Should go back 
#' further. Maybe those variables weren't in use before then? 

# Set parameters
years <- as.list(2010:2022)
vars <- paste0(variables, collapse = ',')
states <- paste0(state_codes$fips, collapse = ',')

# Map over list of years to gather data for each
out <- map(years, \(year){
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
get_str(out)



## Wrangle -----------------------------------------------------------------


# Combine years, convert to numeric, swap census names for our names
dat <- out %>% 
  keep(is.data.frame) %>% 
  bind_rows() %>% 
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
    GEO_ID,
    NAME,
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
  )
get_str(dat)

# Save it
results$acs5 <- dat



## Metadata ----------------------------------------------------------------


vars <- results$acs5$variable_name %>% 
  unique %>% 
  sort
vars

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
  resolution = 'county',
  year = paste0(unique(results$acs5$year), collapse = '|'),
  updates = "5 years",
  source = 'U.S. Census Bureau, American Community Survey: 5-Year Estimates: Detailed Tables, 2022',
  url = 'https://www.census.gov/data/developers/data-sets/acs-5year.html',
  warehouse = FALSE
) %>% 
  add_citation()

# [] consider fixing up citation function

get_str(metas$acs5)
metas$acs5
metas$acs5$citation[1]



# Gini Index --------------------------------------------------------------


gini_vars <- c('B19083_001E', "B19083_001M")

# Set parameters
years <- as.list(seq(2010, 2022, 1))
vars <- paste0(gini_vars, collapse = ',')
states <- paste0(state_codes$fips, collapse = ',')

# Map over list of years to gather data for each
out <- map(years, \(year){
  get_census_data(
    survey_year = year,
    survey = 'acs/acs5',
    vars = vars,
    county = '*',
    state = states
  )
})
get_str(out)


# Clean each set individually, then combine
results$gini <- out %>% 
  keep(is.data.frame) %>% 
  map(\(df) {
    df %>% 
      rename(
        value = B19083_001E,
        margin = B19083_001M
      ) %>%
      mutate(
        variable_name = 'gini'
      ) %>% 
      select(-c(
        GEO_ID, NAME, state, county
      ))
  }) %>% 
  bind_rows()
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
  resolution = 'county',
  year = paste0(unique(results$gini$year), collapse = '|'),
  updates = "5 years",
  source = 'U.S. Census Bureau, American Community Survey: 5-Year Estimates: Detailed Tables, 2022.',
  url = 'https://www.census.gov/data/developers/data-sets/acs-5year.html',
  warehouse = FALSE
) %>% 
  add_citation()

get_str(metas$gini)



# Unemployment ------------------------------------------------------------


gini_vars <- c('B19083_001E', "B19083_001M")

# Set parameters
years <- as.list(seq(2010, 2022, 1))
vars <- paste0(gini_vars, collapse = ',')
states <- paste0(state_codes$fips, collapse = ',')

# Map over list of years to gather data for each
out <- map(years, \(year){
  get_census_data(
    survey_year = year,
    survey = 'acs/acs5',
    vars = vars,
    county = '*',
    state = states
  )
})
get_str(out)


# Clean each set individually, then combine
results$gini <- out %>% 
  keep(is.data.frame) %>% 
  map(\(df) {
    df %>% 
      rename(
        value = B19083_001E,
        margin = B19083_001M
      ) %>%
      mutate(
        variable_name = 'gini'
      ) %>% 
      select(-c(
        GEO_ID, NAME, state, county
      ))
  }) %>% 
  bind_rows()
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
  resolution = 'county',
  year = paste0(unique(results$gini$year), collapse = '|'),
  updates = "5 years",
  source = 'U.S. Census Bureau, American Community Survey: 5-Year Estimates: Detailed Tables, 2022.',
  url = 'https://www.census.gov/data/developers/data-sets/acs-5year.html',
  warehouse = FALSE
) %>% 
  add_citation()

get_str(metas$gini)



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