# BLS QCEW
# 2024-10-23

# Pulling QCEW data from BLS. This is source of NAICS labor data in FAME warehouse



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  blsAPI,
  jsonlite,
  httr,
  glue,
  snakecase,
  readxl
)

source('3_functions/add_citation.R')
source('3_functions/check_n_records.R')
fips_key <- readRDS('5_objects/fips_key.rds')
naics_key <- readRDS('5_objects/naics_key.rds')

# Results list
results <- list()
metas <- list()



# QCEW --------------------------------------------------------------------
## Pull All Counties -------------------------------------------------------



# Try mapping over several counties
out <- map(fips_key$fips, \(fips) {
  
  tryCatch({
    blsQCEW(
      method = 'Area',
      year = '2023',
      quarter = 'a',
      area = fips
    )
  }, error = function(e) {
    cat("Error: ", e$message, "\n")
    return(
      list(
        error_message = e$message,
        fips = fips
      )
    )
  })
})

get_str(out)
# Works!

# Save this as intermediate object so we don't have to call API
saveRDS(out, '5_objects/api_outs/bls_qcew.rds')



## Clean -------------------------------------------------------------------


out <- readRDS('5_objects/api_outs/bls_qcew.rds')

# Bind, then filter to relevant industries
dat <- out %>% 
  keep(is.data.frame) %>% 
  bind_rows() %>% 
  mutate(
    area_fips = as.character(area_fips),
    area_fips = ifelse(
      str_length(area_fips) == 4, 
      paste0('0', area_fips), 
      area_fips
    )
  ) %>% 
  filter(industry_code %in% naics_key$naics)
get_str(dat)

# Remove irrelevant columns
results$qcew <- dat %>% 
  select(-c(
    own_code, agglvl_code, size_code, qtr
  )) %>% 
  unite(
    col = 'disclosure',
    matches('disclosure'),
    sep = ''
  ) %>% 
  pivot_longer(
    cols = !c('area_fips', 'industry_code', 'year', 'disclosure'),
    names_to = 'variable_name',
    values_to = 'value'
  ) %>%
  mutate(
    disclosure = ifelse(disclosure == '', NA, 'N'),
    variable_name = paste0(
      snakecase::to_lower_camel_case(variable_name),
      'NAICS',
      industry_code
    ),
    .keep = 'unused'
  ) %>% 
  rename(fips = area_fips)
get_str(results$qcew)

results$qcew$variable_name %>% unique %>% sort



## Metadata ----------------------------------------------------------------


(vars <- dat$variable_name %>% unique %>% sort)

# Join qcew data to the NAICS key
qcew_fields <- read_csv('1_raw/bls/naics-based-annual-layout.csv')
metas$qcew <- results$qcew %>% 
  mutate(
    variable_name = str_remove_all(variable_name, 'NAICS[0-9]*') %>% 
      snakecase::to_snake_case()
  ) %>% 
  select(variable_name) %>% 
  unique() %>% 
  left_join(qcew_fields, by = join_by(variable_name == field_name)) %>% 
  select(
    variable_name,
    definition = field_description
  ) %>% 
  mutate(
    variable_name = snakecase::to_lower_camel_case(variable_name)
  )

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
    metric = variable_name, # fix this eventually...
    units = case_when(
      str_detect(variable_name, 'Estabs') ~ 'ratio',
      str_detect(variable_name, 'Emplvl') ~ 'count',
      str_detect(variable_name, 'Wage|AnnualPay') ~ 'usd',
      str_detect(variable_name, '^lq') ~ 'ratio',
      str_detect(variable_name, '^oty') ~ 'percentage',
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
  add_citation()
  
get_str(metas$qcew)



# BLS API -----------------------------------------------------------------


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



# BLS Bulk ----------------------------------------------------------------


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
  filter(fips %in% fips_key$fips) %>% 
  
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
vars <- unemp$variable_name %>% 
  unique %>% 
  sort
vars
# Not sure if metro, influence codes belong in an indicator, best separate?

metas$unemp <- data.frame(
  dimension = rep('economics', 9),
  index = rep('community economy', 9),
  indicator = c(
    'employee numbers',
    rep('wealth/income distribution', 3),
    rep(NA, 2),                                   # revisit this at some point
    rep('wealth/income distribution', 2),
    NA
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
  resolution = rep('county', 9),
  year = c(
    rep(paste0(c(2000:2022), collapse = '|'), 2),
    rep('2021', 2),
    rep('2013', 2),
    rep(paste0(c(2000:2022), collapse = '|'), 2),
    '2013'
  ),
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
)

get_str(metas$unemp)
metas$unemp



# Save and Clear ----------------------------------------------------------


# Wrangle data
get_str(results)
map(results, get_str)
result <- map(results, ~ mutate(.x, year = as.character(year))) %>% 
  bind_rows()
get_str(result)

# Wrangle metadata
get_str(metas)
meta <- bind_rows(metas)
get_str(meta)
head(meta)

# Check results
try(check_n_records(result, meta, 'BLS'))
print('BLS will not line up because of NAICS codes')

saveRDS(result, '5_objects/metrics/bls.RDS')
saveRDS(meta, '5_objects/metadata/bls_meta.RDS')
