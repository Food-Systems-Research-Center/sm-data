# BLS QCEW
# 2024-10-23

# Pulling QCEW data from BLS. This is source of NAICS labor data in FAME
# warehouse Playing around with BLS API, but then using a bulk BLS download for
# the most part. Should update at some point to lean into API.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  # blsAPI,
  jsonlite,
  httr,
  glue,
  snakecase,
  readxl,
  tidyr,
  stringr,
  readr,
  purrr
)

source('3_functions/metadata_utilities.R')
fips_key <- readRDS('5_objects/fips_key.rds')
state_key <- readRDS('5_objects/state_key.rds')
naics_key <- readRDS('5_objects/naics_key.rds')
all_fips <- readRDS('5_objects/all_fips.rds')

# Results list
results <- list()
metas <- list()



# QCEW --------------------------------------------------------------------
## Pull Counties and States -----------------------------------------------


# Get a list of NE counties and all US states
fips_list <- c(
  fips_key$fips[str_length(fips_key$fips) == 5],
  paste0(state_key$state_code, '000')
) %>% 
  unique()

# # Map over whole list. Just 2023 apparently for now
# out <- map(fips_list, \(fips) {
# 
#   tryCatch({
#     blsQCEW(
#       method = 'Area',
#       year = '2023',
#       quarter = 'a',
#       area = fips
#     )
#   }, error = function(e) {
#     cat("Error: ", e$message, "\n")
#     return(
#       list(
#         error_message = e$message,
#         fips = fips
#       )
#     )
#   })
# })
# 
# get_str(out)
# Works!

# # Save this as intermediate object so we don't have to call API
# saveRDS(out, '5_objects/api_outs/bls_qcew.rds')



## Clean -------------------------------------------------------------------


out <- readRDS('5_objects/api_outs/bls_qcew.rds')
get_str(out)

# Clean up each county dataframe, then bind together
dat <- out %>%
  keep(is.data.frame) %>% 
  map(\(x) {
    x %>% 
      filter(
        industry_code %in% naics_key$naics
        ) %>% 
      mutate(
        across(everything(), as.character),
        
        # If fips has a missing leading 0 (length 4), add the leading 0 back
        area_fips = ifelse(
          str_length(area_fips) == 4,
          paste0('0', area_fips),
          area_fips
        ),
        
        # If area fips is a state (ends in 000), remove the 000
        area_fips = str_remove(area_fips, '000')
          
      ) %>%
      pivot_longer(
        cols = c(
          annual_avg_estabs:last_col(),
          -matches('disclosure')
        ),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      filter(if_all(contains("disclosure"), ~ . != "N")) %>% 
      mutate(variable_name = paste(variable_name, industry_code, sep = "_")) %>%
      select(-c(
        industry_code,
        matches('disclosure')
      ))
  }) %>% 
  bind_rows()
get_str(dat)

# Remove irrelevant columns
results$qcew <- dat %>% 
  select(-c(
    own_code, agglvl_code, size_code, qtr
  )) %>% 
  mutate(
    # disclosure = ifelse(disclosure == '', NA, 'N'),
    variable_name = paste0(
      snakecase::to_lower_camel_case(variable_name),
      'NAICS'
    ),
    .keep = 'unused'
  ) %>% 
  rename(fips = area_fips)
get_str(results$qcew)

results$qcew$variable_name %>% unique %>% sort

# Let's just get rid of disclosure codes actually
# Could deal with these later...
results$qcew <- results$qcew %>% 
  filter(str_detect(variable_name, 'DisclosureCode', negate = TRUE))
get_str(results$qcew)



## Metadata ----------------------------------------------------------------


(vars <- results$qcew$variable_name %>% unique %>% sort)

# Join qcew data to the NAICS key
# Keeping both old var name and new var name
qcew_fields <- read_csv('1_raw/bls/naics-based-annual-layout.csv')
metas$qcew <- results$qcew %>% 
  mutate(
    og_variable_name = str_remove_all(variable_name, '[0-9]*NAICS') %>% 
      snakecase::to_snake_case()
  ) %>% 
  select(variable_name, og_variable_name) %>% 
  unique() %>% 
  left_join(qcew_fields, by = join_by(og_variable_name == field_name)) %>% 
  select(
    variable_name,
    og_variable_name,
    definition = field_description
  ) %>% 
  mutate(
    variable_name = snakecase::to_lower_camel_case(variable_name) %>% 
      str_replace_all('Naics', 'NAICS')
  )
metas$qcew

# Check og vars
(og_vars <- metas$qcew$og_variable_name %>% unique %>% sort)

# Made regular metadata file
metas$qcew <- metas$qcew %>% 
  mutate(
    dimension = 'economics',
    index = 'community economy',
    indicator = case_when(
      str_detect(variable_name, 'Emplvl') ~ 'job availability',
      str_detect(variable_name, 'Estabs') ~ 'business failure rate',
      str_detect(variable_name, 'Wage|Pay') ~ 'wage rate',
      .default = NA
    ),
    axis_name = variable_name, # fix this eventually...
    metric = variable_name,
    units = case_when(
      str_detect(variable_name, 'Estabs') ~ 'ratio',
      str_detect(variable_name, 'Emplvl') ~ 'count',
      str_detect(variable_name, 'Wage|AnnualPay|Contribut') ~ 'usd',
       str_detect(variable_name, '^lq') ~ 'ratio',
      str_detect(variable_name, '^oty.*PctChg') ~ 'percentage',
    ),
    annotation = 'disclosure',
    scope = 'national',
    resolution = 'county',
    year = '2023',
    updates = 'annual',
    warehouse = FALSE,
    source = 'U.S. Bureau of Labor Statistics, Quarterly Census of Employment and Wages (2023)',
    url = 'https://www.bls.gov/cew/'
  ) %>% 
  add_citation(access_date = '2024-11-05') %>%
  select(-og_variable_name)
  
get_str(metas$qcew)
try(check_n_records(results$qcew, metas$qcew, 'QCEW'))




# BLS API -----------------------------------------------------------------


## Comment this out only to run it - no accidental queries


# seriesid='BDS0000000000300115110001LQ5'
# 
# url <- glue(
#   'https://api.bls.gov/publicAPI/v2/timeseries/data/',
#   '{seriesid}'
# )
# 
# response <- GET(url, add_headers('Content-Type=application/x-www-form-urlencoded'))
# get_str(response)
# 
# out <- content(response, as = 'text') %>%
#   fromJSON()
# get_str(out$Results$series$data[[1]])

# # This works too if we know series ID up front



# BLS Bulk Unemployment ---------------------------------------------------


# Using unemployment and household income data from BLS, bulk download
# https://www.ers.usda.gov/data-products/county-level-data-sets/county-level-data-sets-download-data/

bls_bulk <- read_xlsx(
  '1_raw/bls/Unemployment.xlsx',
  sheet = 1,
  skip = 4
)

get_str(bls_bulk)
names(bls_bulk) %>% 
  sort()

# Cleaning
results$unemp <- bls_bulk %>% 
  select(-c(Area_Name, State)) %>% 
  rename(fips = FIPS_Code) %>% 
  
  # Change fips to match our system (2-digit codes for state and US)
  # Then we can filter by fips key to get NE states and counties
  mutate(fips = case_when(
    str_detect(fips, '^.{2}000') ~ str_sub(fips, start = 1, end = 2),
    .default = fips 
  )) %>% 
  
  # Get names to match our system, then pivot longer, split name and year
  setNames(c(to_lower_camel_case(names(.)))) %>% 
  pivot_longer(
    cols = -fips,
    names_to = c("variable_name", "year"),
    names_pattern = "^(.*)(\\d{4})$"
  ) %>% 
  mutate(
    variable_name = case_when(
      variable_name == 'medHhIncomePercentOfStateTotal' ~ 'medHhIncomePercState',
      variable_name == 'medianHouseholdIncome' ~ 'medHhIncome',
      variable_name == 'civilianLaborForce' ~ 'civLaborForce',
      .default = variable_name
    ),
    disclosure = NA
  )

get_str(results$unemp)



## Metadata ----------------------------------------------------------------


# Unique variable names
(vars <- get_vars(results$unemp))
# Not sure if metro, influence codes belong in an indicator, best separate?

metas$unemp <- data.frame(
  dimension = c(
    rep('economics', 4),
    rep('utilities', 2),
    rep('economics', 2),
    'utilities'
  ),
  index = c(
    rep('community economy', 4),
    rep('utilities_index', 2),
    rep('community economy', 2),
    'utilities_index'
  ),
  indicator = c(
    'employee numbers',
    rep('wealth/income distribution', 3),
    rep('utilities_indicator', 2),  # revisit this at some point
    rep('wealth/income distribution', 2),
    'utilities_indicator'
  ),
  axis_name = c(
    'Civ Labor Force',
    'Number Employed',
    'Median HH Income',
    'Median HH Income (% of state avg)',
    'Metro Area',
    'Rural Urban Continuum',
    'Number Unemployed',
    'Unemployment Rate (%)',
    'Urban Influence'
  ),
  metric = c(
    'Civilian Labor Force',
    'Number Employed',
    'Median Household Income',
    'Median Household Income (% of State Avg)',
    'Metropolitan Area',
    'Rural Urban Continuum Code',
    'Number unemployed',
    'Unemployment Rate',
    'Urban Influence Code'
  ), 
  variable_name = vars,
  definition = c(
    paste(
      'Number of civilians age 16 or older who are classified either as employed or unemployed.',
      'Conceptually, the labor force is the number of people who are either working or actively looking for work.'
    ),
    paste(
      'People are classified as employed if during the survey reference week, they meet ANY of the following criteria:',
      '1. Worked at least 1 hour as paid employee,',
      '2. Worked at least 1 hour in own business (self-employed),',
      '3. Temporarily absent from job, whether or not they were paid for time off,',
      '4. Worked without pay for a minimum of 15 hours in family owned business.'
    ),
    'Median household income.',
    'Median household income as a percentage of average median household income for the state.',
    'Coded between 1 and 3 on Rural-Urban Continuum (county in metro area and population of 250,000 or more). See URL for details.',
    'Scale from 1 (county in metro area and population of 1 million or more) to 9 (urban population of 5,000 or fewer, not adjacent to metro area). See URL for details.',
    paste0(
      'Meet all of the following criteria:',
      '1. Not employed during survey reference week.',
      '2. Available for work during survey reference week, except for temporary illness.',
      '3. Made at least one specific, active effort to find a job during 4-week period ending with survey reference week.'
    ),
    'Number of unemployed people as percentage of labor force: (Unemployed / Labor Force) * 100',
    'Scale of 1 (metropolitan county, 1 million ort more residents) to 12 (noncore not adjacent to metro or micro area, does nto contain town of 2,500 residents or more). See URL for details)'
  ),
  units = c(
    'count',
    'count',
    'usd',
    'percentage',
    'binary',
    'categorical',
    'count',
    'percentage',
    'categorical'
  ),
  annotation = rep(NA, 9),
  scope = rep('national', 9),
  resolution = get_resolution(results$unemp),
  year = get_all_years(results$unemp),
  latest_year = get_max_year(results$unemp),
  updates = c(
    rep('annual', 4),
    rep(NA, 2),
    rep('annual', 2),
    NA
  ),
  warehouse = rep(FALSE, 9),
  source = c(
    rep('U.S. Department of Labor, Bureau of Labor Statistics, Local Area Unemployment Statistics (LAUS)', 2),
    rep('U.S. Department of Commerce, Bureau of the Census, Small Area Income and Poverty Estimates (SAIPE) Program', 2),
    rep('U.S. Department of of Agriculture, Economic Research Service', 2),
    rep('U.S. Department of Labor, Bureau of Labor Statistics, Local Area Unemployment Statistics (LAUS)', 2),
    'U.S. Department of of Agriculture, Economic Research Service'
  ),
  url = c(
    rep('https://www.bls.gov/lau/tables.htm', 2),
    rep('https://www.census.gov/programs-surveys/saipe.html', 2),
    rep('https://www.ers.usda.gov/topics/rural-economy-population/rural-classifications/', 2),
    rep('https://www.bls.gov/lau/tables.htm', 2),
    'https://www.ers.usda.gov/topics/rural-economy-population/rural-classifications/'
  )
) %>% 
  add_citation(access_date = '2024-11-05')

get_str(metas$unemp)
metas$unemp



# BLS Bulk Farm Income ----------------------------------------------------


# https://www.ers.usda.gov/data-products/farm-income-and-wealth-statistics/data-files-u-s-and-state-level-farm-income-and-wealth-statistics/
# Getting returns to operators by state
dat <- read.csv('1_raw/bls/FarmIncome_WealthStatisticsData_September2024.csv')
get_str(dat)

# Pull only relevant variables - for data or metadata
clean <- dat %>% 
  filter(VariableDescriptionPart2 == 'Returns to operators') %>% 
  select(
    year = Year,
    state = State,
    description = VariableDescriptionTotal,
    Amount,
    Source
  )
get_str(clean)

# Arrange data by state
farm_income <- clean %>% 
  select(year, state, value = Amount) %>% 
  mutate(
    value = value * 1000,
    variable_name = 'netReturnOperators'
  ) 
get_str(farm_income)

# Join to fips key to to add fips and lose state names
# Also filter to 2000 or later
farm_income <- state_key %>% 
  select(state, state_code) %>% 
  right_join(farm_income) %>% 
  filter(year >= 2000) %>% 
  select(-state) %>% 
  rename(fips = state_code)
get_str(farm_income)

# Join to results list
results$farm_income <- farm_income



## Metadata ----------------------------------------------------------------


get_str(clean)

metas$farm_income <- data.frame(
  dimension = 'economics',
  index = 'farmer personal finance',
  indicator = 'operator salary/wage',
  axis_name = 'Net Return to Operators ($)',
  variable_name = unique(results$farm_income$variable_name),
  metric = 'Net return to operators',
  definition = unique(clean$description),
  units = 'usd',
  annotation = NA,
  scope = 'national',
  resolution = 'state',
  year = get_all_years(results$farm_income),
  latest_year = get_max_year(results$farm_income),
  updates = 'annual',
  warehouse = FALSE,
  source = 'U.S. Department of Agriculture, Economic Research Service, Farm Income and Wealth Statistics',
  url = 'https://www.ers.usda.gov/data-products/farm-income-and-wealth-statistics/data-files-u-s-and-state-level-farm-income-and-wealth-statistics/'
)

get_str(metas$farm_income)
metas$farm_income

check_n_records(results$farm_income, metas$farm_income, 'Farm Income')



# Save and Clear ----------------------------------------------------------


# Wrangle data
get_str(results)
map(results, get_str)
result <- map(results, ~ {
  .x %>% 
    mutate(
      year = as.character(year),
      value = as.character(value)
    )
}) %>% 
  bind_rows()
get_str(result)

# Wrangle metadata
get_str(metas)
meta <- bind_rows(metas)
get_str(meta)
head(meta)

# Check results
try(check_n_records(result, meta, 'BLS'))

saveRDS(result, '5_objects/metrics/bls.RDS')
saveRDS(meta, '5_objects/metadata/bls_meta.RDS')

clear_data()
gc()
