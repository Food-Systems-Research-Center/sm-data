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
source('3_functions/get_census_data.R')
source('3_functions/filter_fips.R')
source('3_functions/add_citation.R')
source('3_functions/check_n_records.R')

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
  year = paste0(results$acs5$year, collapse = '|'),
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



# # Wage From Bulk ----------------------------------------------------------
# 
# 
# acs <- read_csv('1_raw/census/ACSST1Y2023.S2411_2024-10-03T114322/ACSST1Y2023.S2411-Data.csv')
# get_str(acs)
# 
# variables <- c(
#   'S2411_C02_023E',
#   'S2411_C02_030E',
#   'S2411_C03_023E',
#   'S2411_C03_030E',
#   'S2411_C04_023E',
#   'S2411_C04_030E'
# )
# 
# results$wage_rate <- acs %>% 
#   select(
#     GEO_ID,
#     NAME,
#     all_of(variables)
#   ) %>% 
#   rename(
#     "median_earnings_male_food_prep_and_serving" = 'S2411_C02_023E',
#     "median_earnings_male_farming_fishing_forestry" = 'S2411_C02_030E',
#     "median_earnings_female_food_prep_and_serving" = 'S2411_C03_023E',
#     "median_earnings_female_farming_fishing_and_forestry" = 'S2411_C03_030E',
#     "womens_earnings_as_perc_of_men_food_prep_and_serving" = 'S2411_C04_023E',
#     "womens_earnings_as_perc_of_men_farming_fishing_forestry" = 'S2411_C04_030E'
#   ) %>% 
#   slice(-1) %>% 
#   filter(str_detect(GEO_ID, 'US$', negate = TRUE)) %>% 
#   mutate(
#     fips = str_split_i(GEO_ID, 'US', 2),
#     year = 2023,
#     across(everything(), ~ str_replace(., '-', NA_character_))
#   ) %>% 
#   filter(fips %in% fips_all) %>%
#   select(-c(GEO_ID, NAME)) %>% 
#   pivot_longer(
#     cols = !c(fips, year),
#     names_to = 'variable_name',
#     values_to = 'value'
#   )
# 
# get_str(results$wage_rate)


## Metadata ----------------------------------------------------------------

# 
# # Reminder of names and order
# vars <- results$wage_rate$variable_name %>% unique %>% sort
# 
# # Save metadata
# metas$wage_rate <- tibble(
#   dimension = "economics",
#   index = 'community economy',
#   indicator = 'wage rate',
#   metric = c(
#     'Median wage, female, farming fishing and forestry',
#     'Median wage, female, food service',
#     'Median wage, male, farming fishing and forestry',
#     'Median wage, male, food service',
#     'Female earnings as percentage of male, farming fishing forestry',
#     'Female earnings as percentage of male, food service'
#   ),
#   definition = c(
#     'Median earnings for female, Civilian employed population 16 years and over with earnings, Farming, fishing, and forestry occupations',
#     'Median earnings for female, Civilian employed population 16 years and over with earnings, Food preparation and serving related occupations',
#     'Median earnings for male, Civilian employed population 16 years and over with earnings, Farming, fishing, and forestry occupations',
#     'Median earnings for male, Civilian employed population 16 years and over with earnings, Food preparation and serving related occupations',
#     'Female earnings as a percentage of male earnings, Civilian employed population 16 years and over with earnings, Farming, fishing, and forestry occupations',
#     'Female earnings as a percentage of male earnings, Civilian employed population 16 years and over with earnings, Food preparation and serving related occupations'
#   ),
#   variable_name = c(
#     vars
#   ),
#   units = c(
#     rep('dollars', 4),
#     rep('percentage', 2)
#   ),
#   scope = 'national',
#   resolution = 'county',
#   latest_year = '2023',
#   all_years = paste0(2015:2023, collapse = ','),
#   updates = "annual",
#   quality = '2',
#   source = "Source: U.S. Census Bureau, 2023 American Community Survey 1-Year Estimates",
#   url = 'https://data.census.gov/table?q=S2411&g=010XX00US$0500000&y=2023',
#   citation = 
#   '@misc{Census2023ACSST1Y2023.S2411,
#     author={U.S. Census Bureau, U.S. Department of Commerce},
#     title={Occupation by Sex and Median Earnings in the Past 12 Months (in 2023 Inflation-Adjusted Dollars) for the Civilian Employed Population 16 Years and Over},
#     vintage=2023,
#     howpublished={U.S. Census Bureau},
#     url={https://data.census.gov/table/ACSST1Y2023.S2411?q=S2411: OCCUPATION BY SEX AND MEDIAN EARNINGS IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS) FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER&g=010XX00US,$0500000&y=2023},
#     note={Accessed on 3 October 2024}
#   }',
#   warehouse = FALSE
# )



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

