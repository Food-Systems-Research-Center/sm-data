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

# county fips for New England (differences for CT restructuring)
fips_2021 <- readRDS('5_objects/fips_2021.rds')
fips_2024 <- readRDS('5_objects/fips_2024.rds')
fips_all <- readRDS('5_objects/fips_all.rds')
state_codes <- readRDS('5_objects/ne_state_codes.rds')

# lists of results
results <- list()
metas <- list()



# ACS 5-year --------------------------------------------------------------
## Variables ---------------------------------------------------------------


# All relevant variables for American Community Survey data
variables <- list(
  # Education
  'education_total' = 'B15003_001E',
  'education_hs' = 'B15003_017E',
  'education_ged' = 'B15003_018E',
  'education_bs' = 'B15003_022E',
  
  # Housing
  'n_housing_units' = 'B25001_001E',
  # 'total_occupancy_status' = 'B25002_001E',
  'n_occupied' = 'B25002_002E',
  'n_vacant' = 'B25002_003E',
  
  # Rent by bedrooms
  'median_rent_1br' = 'B25031_003E',
  'median_rent_4br' = 'B25031_006E',
  
  # Housing age
  'median_construction_year' = 'B25035_001E',
  
  # More rent
  'median_rent' = 'B25064_001E',
  'median_rent_as_perc_of_household_income' = 'B25071_001E'
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



## Wrangle -----------------------------------------------------------------


year <- 2022
survey <- 'acs/acs5'
vars <- paste0(variables, collapse = ',')
county <- '*'
state <- paste0(state_codes$fips, collapse = ',')

url <- glue(
  'https://api.census.gov/data/{year}/{survey}',
  '?get=GEO_ID,NAME,{vars}',
  '&for=county:{county}',
  '&in=state:{state}'
  # '?key={api_key}',
)
url

out <- GET(url) %>% 
  content(as = 'text') %>%
  fromJSON()
get_str(out)

# Clean it up
dat <- out %>%
  as.data.frame() %>%
  row_to_names(1) %>%
  mutate(
    fips = paste0(state, county),
    year = year
  ) %>%
  select(-state, -county) %>%
  filter(fips %in% fips_all)
get_str(dat)

# Fix up names
names(dat) <- recode(names(dat), !!!crosswalk)
get_str(dat)

# Calculations to get proportions for education, housing
dat <- dat %>% 
  mutate(
    across(c(2:median_rent_as_perc_of_household_income), as.numeric),
    education_prop_hs_ged = (education_hs + education_ged) / education_total,
    education_prop_bs = education_bs / education_total,
    vacancy_rate = n_vacant / n_housing_units
  ) %>% 
  select(-c(
    education_total:education_bs,
    n_occupied,
    n_vacant,
    GEO_ID,
    NAME
  )) %>% 
  select(fips, year, everything())
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
    rep("health", 9)
  ),
  index = c(
    rep('education', 2),
    rep('physical health', 7)
  ),
  indicator = c(
    rep('educational attainment', 2),
    rep('housing supply and quality', 7)
  ),
  metric = c(
    'Proportion with bachelor\'s degree',
    'Proportion with high school diploma, GED, or equivalent',
    'Median year of construction',
    'Median rent',
    'Median rent, 1br',
    'Median rent, 4br',
    'Median rent as proportion of income',
    'Total number of housing units',
    'Vacancy rate'
  ),
  definition = c(
    'Proportion of population age 25 or older with bachelor\'s degree',
    'Proportion of population age 25 or older with high school diploma, GED, or equivalent',
    'Median year in which housing unit was built',
    'Median gross rent, aggregate',
    'Median gross rent for a 1-bedroom apartment',
    'Median gross rent for a 4-bedroom apartment',
    'Median gross rent as a percentage of household income over the last 12 months',
    'Total number of housing units',
    'Vacancy rate, rentals'
  ),
  variable_name = c(
    vars
  ),
  units = c(
    rep('proportion', 2),
    'year',
    rep('dollars', 3),
    'percentage',
    'count',
    'proportion'
  ),
  scope = 'national',
  resolution = 'county',
  latest_year = '2023',
  all_years = paste0(2009:2023, collapse = ','),
  updates = "annual",
  quality = '2',
  source = c(
    rep('U.S. Census Bureau, American Community Survey: 5-Year Estimates: Detailed Tables, 2022', 9)
  ),
  url = c(
    rep('https://www.census.gov/data/developers/data-sets/acs-5year.html', 9)
  ),
  citation = c(
    rep('U.S. Census Bureau, “American Community Survey: 5-Year Estimates: Detailed Tables,” 2022, <http://api.census.gov/data/2022/acs/acs5>, accessed on October 3, 2024.', 9)
  ),
  warehouse = FALSE
)

get_str(metas$acs5)
metas$acs5


# Wage Rates --------------------------------------------------------------


# Pulling this as updated version of warehouse data
# variables <- list(
  # "median_earnings_for_male_food_preparation_and_serving_related_occupations" = 'S2411_C02_023E',
  # "median_earnings_for_female_food_preparation_and_serving_related_occupations" = 'S2411_C02_030E',
  # "women's_earnings_as_a_percentage_of_men's_earning_food_preparation_and_serving_related_occupations" = 'S2411_C03_023E',
  # "median_earnings_for_male_farming_fishing_and_forestry_occupations" = 'S2411_C03_030E',
  # "median_earnings_for_female_farming_fishing_and_forestry_occupations" = 'S2411_C04_023E',
  # "women's_earnings_as_a_percentage_of_men's_earning_farming_fishing_and_forestry_occupations" = 'S2411_C04_030E'
# )
# 
# base <- 'https://api.census.gov/data/'
# year <- 2022
# survey <- '/acs/acs1'
# name <- '?get=NAME,'
# county <- '*'
# # state <- paste0(state_codes, collapse = ',')
# state <- '*'
# vars <- paste0(variables, collapse = ',')
# 
# url <- glue(
#   base,
#   year,
#   survey,
#   '?get=NAME,{vars}',
#   # '&for=county:{county}',
#   # '&for=state:{state}'
#   # '?key={api_key}',
# )
# url
# 
# 
# out <- GET(url) %>% 
#   content(as = 'text') %>%
#   fromJSON()
# get_str(out)
# 
# dat <- out %>% 
#   as.data.frame() %>% 
#   row_to_names(1) %>% 
#   mutate(
#     fips = paste0(state, county),
#     year = year
#   ) %>% 
#   select(-state, -county) %>% 
#   filter(fips %in% fips_all)
# get_str(dat)



# Wage From Bulk ----------------------------------------------------------



acs <- read_csv('1_raw/census/ACSST1Y2023.S2411_2024-10-03T114322/ACSST1Y2023.S2411-Data.csv')
get_str(acs)

variables <- c(
  'S2411_C02_023E',
  'S2411_C02_030E',
  'S2411_C03_023E',
  'S2411_C03_030E',
  'S2411_C04_023E',
  'S2411_C04_030E'
)

results$wage_rate <- acs %>% 
  select(
    GEO_ID,
    NAME,
    all_of(variables)
  ) %>% 
  rename(
    "median_earnings_male_food_prep_and_serving" = 'S2411_C02_023E',
    "median_earnings_male_farming_fishing_forestry" = 'S2411_C02_030E',
    "median_earnings_female_food_prep_and_serving" = 'S2411_C03_023E',
    "median_earnings_female_farming_fishing_and_forestry" = 'S2411_C03_030E',
    "womens_earnings_as_perc_of_men_food_prep_and_serving" = 'S2411_C04_023E',
    "womens_earnings_as_perc_of_men_farming_fishing_forestry" = 'S2411_C04_030E'
  ) %>% 
  slice(-1) %>% 
  filter(str_detect(GEO_ID, 'US$', negate = TRUE)) %>% 
  mutate(
    fips = str_split_i(GEO_ID, 'US', 2),
    year = 2023,
    across(everything(), ~ str_replace(., '-', NA_character_))
  ) %>% 
  filter(fips %in% fips_all) %>%
  select(-c(GEO_ID, NAME)) %>% 
  pivot_longer(
    cols = !c(fips, year),
    names_to = 'variable_name',
    values_to = 'value'
  )

get_str(results$wage_rate)


## Metadata ----------------------------------------------------------------


# Reminder of names and order
vars <- results$wage_rate$variable_name %>% unique %>% sort

# Save metadata
metas$wage_rate <- tibble(
  dimension = "economics",
  index = 'community economy',
  indicator = 'wage rate',
  metric = c(
    'Median wage, female, farming fishing and forestry',
    'Median wage, female, food service',
    'Median wage, male, farming fishing and forestry',
    'Median wage, male, food service',
    'Female earnings as percentage of male, farming fishing forestry',
    'Female earnings as percentage of male, food service'
  ),
  definition = c(
    'Median earnings for female, Civilian employed population 16 years and over with earnings, Farming, fishing, and forestry occupations',
    'Median earnings for female, Civilian employed population 16 years and over with earnings, Food preparation and serving related occupations',
    'Median earnings for male, Civilian employed population 16 years and over with earnings, Farming, fishing, and forestry occupations',
    'Median earnings for male, Civilian employed population 16 years and over with earnings, Food preparation and serving related occupations',
    'Female earnings as a percentage of male earnings, Civilian employed population 16 years and over with earnings, Farming, fishing, and forestry occupations',
    'Female earnings as a percentage of male earnings, Civilian employed population 16 years and over with earnings, Food preparation and serving related occupations'
  ),
  variable_name = c(
    vars
  ),
  units = c(
    rep('dollars', 4),
    rep('percentage', 2)
  ),
  scope = 'national',
  resolution = 'county',
  latest_year = '2023',
  all_years = paste0(2015:2023, collapse = ','),
  updates = "annual",
  quality = '2',
  source = "Source: U.S. Census Bureau, 2023 American Community Survey 1-Year Estimates",
  url = 'https://data.census.gov/table?q=S2411&g=010XX00US$0500000&y=2023',
  citation = 
  '@misc{Census2023ACSST1Y2023.S2411,
    author={U.S. Census Bureau, U.S. Department of Commerce},
    title={Occupation by Sex and Median Earnings in the Past 12 Months (in 2023 Inflation-Adjusted Dollars) for the Civilian Employed Population 16 Years and Over},
    vintage=2023,
    howpublished={U.S. Census Bureau},
    url={https://data.census.gov/table/ACSST1Y2023.S2411?q=S2411: OCCUPATION BY SEX AND MEDIAN EARNINGS IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS) FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER&g=010XX00US,$0500000&y=2023},
    note={Accessed on 3 October 2024}
  }',
  warehouse = FALSE
)



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

# Save them
saveRDS(result, '5_objects/metrics/census.RDS')
saveRDS(meta, '5_objects/metadata/census_meta.RDS')

