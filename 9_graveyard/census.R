# Census API
# 2024-10-02

# Variables list: https://api.census.gov/data/2023/acs/acs1/variables.html


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr,
  tibble,
  janitor,
  tidyr,
  stringr
)

# Load Census API Key
# census_key <- Sys.getenv('ARMS_API_KEY')

# Crosswalk of variable_names and census codes for relevant variables
# defined in 4_scripts/census_api.R
crosswalk <- readRDS('5_objects/census_acs5_crosswalk.rds')

# lists of results
results <- list()
metas <- list()


# Rework ------------------------------------------------------------------
## Wrangle -----------------------------------------------------------------


out <- readRDS('5_objects/api_outs/acs5_neast_counties_states_2008_2023.rds')
get_str(out)

# Make normal fips column
dat <- out %>% 
  mutate(fips = paste0(state, ifelse(is.na(county), '', county))) %>% 
  select(-c(state, county))
get_str(dat)

# Check ranges for weird values
map(dat, range, na.rm = TRUE)
# median income is hinky, also housing year

# Recode -666666666 to missing for income and 0 to missing for housing year
dat <- dat %>% 
  mutate(
    across(everything(), ~ case_when(
      .x == -666666666 | .x == 0 ~ NA,
      .default = .x
    ))
  )
get_str(dat)           
map(dat, range, na.rm = TRUE)

# Calculations to get proportions for education, housing, earnings
dat <- dat %>% 
  mutate(
    edPercHSGED = ((edTotalHS + edTotalGED) / edTotal) * 100,
    edPercBS = (edTotalBS / edTotal) * 100,
    vacancyRate = (nHousingVacant / nHousingUnits) * 100,
    womenEarnPercMenFFF = (medianFemaleEarningsFFF / medianMaleEarningsFPS) * 100,
    womenEarnPercMenFPS = (medianFemaleEarningsFPS / medianMaleEarningsFPS) * 100
  ) %>% 
  select(-c(starts_with('edTotal')))
get_str(dat)
map(dat, range, na.rm = TRUE)

# Pivot longer
dat <- dat %>% 
  pivot_longer(
    cols = !c(year, fips),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(dat)



## Metadata ----------------------------------------------------------------


# Load from CSV
meta <- read_csv('5_objects/metadata_csv/census_meta.csv')
get_str(meta)

meta <- meta %>% 
  arrange(variable_name) %>% 
  mutate(
    resolution = meta_resolution(dat),
    updates = '5 years',
    latest_year = meta_latest_year(dat),
    years = meta_years(dat)
  ) %>% 
  meta_citation(date = '2025-07-03')
get_str(meta)



# ACS 5-year --------------------------------------------------------------


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

# Calculations for female earnings as proportion of male in FFF and FPS
dat <- dat %>% 
  mutate(
    womenEarnPercMenFFF = (medianFemaleEarningsFFF / medianMaleEarningsFPS) * 100,
    womenEarnPercMenFPS = (medianFemaleEarningsFPS / medianMaleEarningsFPS) * 100
  )
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

# Save metadata
metas$acs5 <- vars %>% 
  as.data.frame() %>% 
  setNames(c('variable_name')) %>% 
  mutate(
    dimension = case_when(
      str_detect(variable_name, 'Earnings|^womenEarn') ~ 'economics',
      str_detect(variable_name, 'Housing|^edPerc|^rent|^vacancy') ~ 'health',
      str_detect(variable_name, 'population') ~ 'utilities',
      .default = NA
    ),
    index = case_when(
      str_detect(variable_name, 'Earnings|^womenEarn') ~ 'community economy',
      str_detect(variable_name, '^edPerc') ~ 'education',
      str_detect(variable_name, '^rent|^vacancy|Housing') ~ 'physical health',
      str_detect(variable_name, 'population') ~ 'utilities_index',
      .default = NA
    ),
    indicator = case_when(
      str_detect(index, 'education') ~ 'educational attainment',
      str_detect(index, 'community economy') ~ 'wealth/income distribution',
      str_detect(index, 'physical health') ~ 'housing supply and quality',
      str_detect(variable_name, 'population') ~ 'utilities_indicator',
      .default = NA
    ),
    metric = c(
      'Percentage with bachelor degree',
      'Percentage with high school diploma, GED, or equivalent',
      "Median earnings (dollars) for female, farming, fishing, and forestry occupations",
      "Median earnings (dollars) for female, food preparation and serving related occupations",
      'Median housing age',
      "Median earnings (dollars) for male, farming, fishing, and forestry occupations",
      "Median earnings (dollars) for male, food preparation and serving related occupations",
      'Number of occupied housing units',
      'Number of total housing units',
      'Number of vacant housing units',
      'Population',
      'Median rent',
      'Median rent, 1br',
      'Median rent, 4br',
      'Median rent as a percentage of income',
      'Rental vacancy rate',
      'Women\'s earnings as a percentage of mens\', farming, fishing, and forestry occupations',
      'Women\'s earnings as a percentage of mens\', food preparation and serving occupations'
    ),
    definition = c(
      'Percentage of population age 25 or older with bachelor\'s degree',
      'Percentage of population age 25 or older with high school diploma, GED, or equivalent',
      'Median earnings (dollars) for female, Civilian employed population 16 years and over with earnings, farming, fishing, and forestry occupations',
      'Median earnings (dollars) for female, Civilian employed population 16 years and over with earnings, food preparation and serving related occupations',
      'Median year in which housing unit was constructed',
      'Median earnings (dollars) for male, Civilian employed population 16 years and over with earnings, farming, fishing, and forestry occupations',
      'Median earnings (dollars) for male, Civilian employed population 16 years and over with earnings, food preparation and serving related occupations',
      'Number of occupied housing units',
      'Number of total housing units',
      'Number of vacant housing units',
      'Population',
      'Median gross rent, aggregate',
      'Median gross rent for a 1-bedroom apartment',
      'Median gross rent for a 4-bedroom apartment',
      'Median gross rent as a percentage of household income over the last 12 months',
      'Vacancy rate, rentals',
      'Womens\' earnings as a percentage of mens\', civilian employed population 16 years and over with earnings, farming, fishing, and forestry occupations',
      'Womens\' earnings as a percentage of mens\', civilian employed population 16 years and over with earnings, food preparation and serving occupations'
    ),
    axis_name = c(
      'Bachelor degree (%)',
      'HS or GED (%)',
      'Median Wage, Female, FFF',
      'Median Wage, Female, FPS',
      'Median housing year',
      'Median Wage, Male, FFF',
      'Median Wage, Male, FPS',
      'n Occupied Units',
      'n Housing Units',
      'n Vacant Units',
      'Population',
      'Median rent',
      'Median rent 1BR',
      'Median rent 4BR',
      'Rent (% of income)',
      'Vacancy rate',
      'Womens\' Earnings FFF (% of Mens\')',
      'Womens\' Earnings FPS (% of Mens\')'
    ),
    units = case_when(
      str_detect(variable_name, 'edPerc|^womenEarn') ~ 'percentage',
      str_detect(variable_name, 'Earnings|rent') ~ 'usd',
      str_detect(variable_name, '^n|population') ~ 'count',
      str_detect(variable_name, 'vacancyRate') ~ 'proportion',
      str_detect(variable_name, 'Year') ~ 'year',
      .default = NA
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
  add_citation(date = '2025-01-08')
    
metas$acs5



# Gini Index --------------------------------------------------------------

# Reload from saved
counties_out <- readRDS('5_objects/api_outs/gini_census_acs5_counties_2010_2022.rds')
states_out <- readRDS('5_objects/api_outs/gini_census_acs5_states_2010_2022.rds')

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



## Metadata ----------------------------------------------------------------


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
