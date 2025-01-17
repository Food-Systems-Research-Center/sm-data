#' Labor Costs


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  readr,
  purrr,
  janitor,
  skimr,
  tidyr
)

# Source functions
source('3_functions/wrangling/pull_variable.R')
source('3_functions/metadata_utilities.R')

# Pull Census of Ag data filtered to New England counties (and all states)
# coa_ne <- readRDS('5_objects/coa_ne.rds')
# coa_ne <- readRDS('1_raw/nass/nass_census_counties_2007-2022.rds')
coa_ne <- readRDS('1_raw/nass/ne_counties_all_states_2007-2022.rds') %>% 
  mutate(fips = paste0(state_fips_code, county_code)) %>% 
  setNames(c(names(.) %>% snakecase::to_snake_case())) %>% 
  mutate(value = as.numeric(str_remove_all(value, ',')))
get_str(coa_ne)
coa_ne$fips %>% unique

# Fips keys
fips_key <- readRDS('5_objects/fips_key.rds')
state_key <- readRDS('5_objects/state_key.rds')
all_fips <- readRDS('5_objects/all_fips.rds')

# Initialize results lists
results <- list()
metas <- list()

# Last time API was called
access_date <- '2025-01-07'



# Economics ---------------------------------------------------------------
## Labor Total -------------------------------------------------------------


# Check labor
labor_vars <- coa_ne$short_desc %>% str_subset('^LABOR') %>% unique() %>% sort



results$labor_costs <- coa_ne %>% 
  filter(
    commodity_desc == 'LABOR',
    domain_desc == 'TOTAL'
  ) %>% 
  
  # New column with the variables we want
  mutate(
    variable_name = case_when(
      short_desc == 'LABOR, HIRED - EXPENSE, MEASURED IN $' ~ 'expHiredLabor',
      short_desc == 'LABOR, HIRED - EXPENSE, MEASURED IN PCT OF OPERATING EXPENSES' ~ 'expHiredLaborPercOpExp',
      short_desc == 'LABOR, HIRED - NUMBER OF WORKERS' ~ 'nHiredWorkers',
      short_desc == 'LABOR, HIRED - OPERATIONS WITH EXPENSE' ~ 'nOpsHiredLaborExp',
      short_desc == 'LABOR, HIRED - OPERATIONS WITH WORKERS' ~ 'nOpsHiredLabor',
      
      short_desc == 'LABOR, HIRED, GE 150 DAYS - NUMBER OF WORKERS' ~ 'nWorkersGE150',
      short_desc == 'LABOR, HIRED, GE 150 DAYS - OPERATIONS WITH WORKERS' ~ 'nOpsWorkersGE150',
      
      short_desc == 'LABOR, HIRED, LT 150 DAYS - NUMBER OF WORKERS' ~ 'nWorkersLE150',
      short_desc == 'LABOR, HIRED, LT 150 DAYS - OPERATIONS WITH WORKERS' ~ 'nOpsWorkersLE150',
      
      short_desc == 'LABOR, MIGRANT - NUMBER OF WORKERS' ~ 'nMigrantWorkers',
      short_desc == 'LABOR, MIGRANT - OPERATIONS WITH WORKERS' ~ 'nOpsMigrantWorkers',
      
      short_desc == 'LABOR, UNPAID - NUMBER OF WORKERS' ~ 'nUnpaidWorkers',
      short_desc == 'LABOR, UNPAID - OPERATIONS WITH WORKERS' ~ 'nOpsUnpaidWorkers'
    )
  ) %>% 
  
  # Pull the relevant columns. metric includes all relevant variables
  select(
    fips,
    county_name,
    state_name,
    year,
    variable_name,
    value
    # value_codes
    # cv_percent
  ) %>% 
  filter(!is.na(variable_name)) %>% 
  
  # Pivot to get average by operation, then put it back to long format
  pivot_wider(
    names_from = 'variable_name',
    values_from = 'value'
  ) %>% 
  mutate(expHiredLaborPF = expHiredLabor / nOpsHiredLabor) %>% 
  pivot_longer(
    cols = !c(fips, county_name, state_name, year),
    names_to = 'variable_name',
    values_to = 'value'
  ) %>% 
  select(
    fips,
    county_name,
    state_name,
    year,
    variable_name,
    value
    # value_codes
    # cv_percent
  )
get_str(results$labor_costs)

# CHECK
results$labor_costs %>% 
  filter(variable_name == 'nOpsHiredLabor') %>% 
  filter(year == '2022') %>% 
  arrange(fips) %>% 
  print(n = 100)

# Reference variables
labor_vars <- results$labor_costs$variable_name %>% 
  unique %>% 
  sort()
labor_vars

## Farm labor metadata
metas$labor_costs <- data.frame(
  dimension = "economics",
  index = 'food business profitability',
  indicator = 'labor costs',
  metric = c(
    "Labor expenses",
    'Labor expenses as percentage of expenses',
    "Labor expenses per farm",
    'Number of hired workers',
    'Number of migrant workers', #
    'Number of operations with hired labor',
    'Number of operations with labor expenses',
    'Number of operations with migrant labor', #
    'Number of operations with unpaid labor',
    'Number of operations with long-term labor',
    'Number of operations with short-term labor',
    'Number of unpaid workers',
    'Number of long-term workers',
    'Number of short-term workers'
  ), 
  variable_name = labor_vars,
  definition = c(
    "Total expenses from hired labor",
    'Hired labor expenses as a percentage of total operating expenses',
    'Hired labor expenses per farm (nOpsHiredLabor/expHiredLabor)',
    'Number of hired workers',
    'Number of migrant workers',
    'Number of operations with any hired labor', #
    'Number of operations with any labor expenses',
    'Number of operations with any migrant labor', 
    'Number of operations with any unpaid labor',
    'Number of operations with any long-term labor, greater than or equal to 150 days',
    'Number of operations with any short-term labor, less than or equal to 150 days',
    'Number of unpaid workers',
    'Number of long-term workers, greater than or equal to 150 days',
    'Number of short-term workers, less than equal to 150 days'
  ),
  axis_name = c(
    "Labor expenses ($)",
    'Labor expense (% of total)',
    "Labor expense per farm ($)",
    'Hired Workers (#)',
    'Migrant workers (#)',
    'Farms with labor (#)',
    'Farms with labor expenses (#)',
    'Farms with migrant labor (#)',
    'Farms with unpaid labor (#)',
    'Farms with long-term labor (#)',
    'Farms with short-term labor (#)',
    'Unpaid workers (#)',
    'Long-term workers (#)',
    'Short-term workers (#)'
  ),
  units = c(
    'usd', 
    'percentage', 
    'usd',
    rep('count', 11)
  ),
  scope = 'national',
  resolution = 'county, state',
  year = '2022',
  latest_year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation(access_date = access_date)

metas$labor_costs


## Total Costs ----------------------------------------------------------


results$total_costs <- coa_ne %>% 
  filter(
    commodity_desc == 'EXPENSE TOTALS',
    domain_desc == 'TOTAL'
  ) %>% 
  
  # New column with the variables we want
  mutate(
    variable_name = case_when(
      short_desc == 'EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $ / OPERATION'
      ~ 'expPF',
    )
  ) %>% 
  
  # Pull the relevant columns. metric includes all relevant variables
  select(
    fips,
    county_name,
    state_name,
    year,
    variable_name,
    value
    # value_codes,
    # cv_percent
  ) %>% 
  filter(!is.na(variable_name))
  
# Write metadata
metas$total_costs <- tibble(
  dimension = "economics",
  index = 'food business profitability',
  indicator = 'total costs',
  metric = c(
    "Expenses per operation"
  ), 
  variable_name = c(
    "expPF"
  ),
  definition = 'Expense totals, oeprating - expense, measured in $ / operation',
  axis_name = 'Expenses per operation ($)',
  units = 'usd',
  scope = 'national',
  resolution = 'county, state',
  year = '2022',
  latest_year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation(access_date = access_date)



## Total Income ------------------------------------------------------------


results$total_income <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'INCOME, FARM-RELATED',
  domain_desc = 'TOTAL',
  short_desc = 'INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $ / OPERATION',
  variable_name = 'farmIncomePF'
)
get_str(results$total_income)

metas$total_income <- tibble(
  dimension = "economics",
  index = 'food business profitability',
  indicator = 'total sales',
  metric = c(
    "Mean farm related income per operation"
  ), 
  variable_name = c(
    "farmIncomePF"
  ),
  axis_name = 'Income per farm ($)',
  definition = 'Income, farm-related - receipts, measured in $ / operation',
  units = 'usd',
  scope = 'national',
  resolution = 'county, state',
  year = '2022',
  latest_year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation(access_date = access_date)



## Area Operated -----------------------------------------------------------


results$total_acres_operated <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'FARM OPERATIONS',
  domain_desc = 'TOTAL',
  short_desc = 'FARM OPERATIONS - ACRES OPERATED',
  variable_name = 'acresOperated'
)
get_str(results$total_acres_operated)

results$acres_per_operation <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'FARM OPERATIONS',
  domain_desc = 'TOTAL',
  short_desc = 'FARM OPERATIONS - AREA OPERATED, MEASURED IN ACRES / OPERATION',
  variable_name = 'acresPF'
)
get_str(results$total_acres_operated)

results$median_acres_per_operation <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'FARM OPERATIONS',
  domain_desc = 'TOTAL',
  short_desc = 'FARM OPERATIONS - AREA OPERATED, MEASURED IN ACRES / OPERATION, MEDIAN',
  variable_name = 'medianAcresPF'
)
get_str(results$median_acres_per_operation)

vars <- c(
  'acresOperated',
  'acresPF',
  'medianAcresPF'
)
  
metas$acres_operated <- tibble(
  dimension = "economics",
  index = 'food business profitability',
  indicator = 'acreage in production',
  metric = c(
    'Total acres operated',
    'Mean acres per farm',
    'Median acres per farm'
  ),
  definition = c(
    'Total acres operated on farm operations',
    'Mean acres operated per farm',
    'Median acres operated per farm'
  ),
  variable_name = vars,
  axis_name = c(
    'Acres operated',
    'Mean acres per farm',
    'Median acres per farm'
  ),
  units = 'acres',
  scope = 'national',
  resolution = 'county, state',
  year = '2022',
  latest_year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation(access_date = access_date)



## Assets ------------------------------------------------------------------


get_str(coa_ne)

results$assets <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'AG LAND',
  domain_desc = 'TOTAL',
  short_desc = 'AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / OPERATION',
  variable_name = 'landValPF'
)
get_str(results$assets)

results$assets_acre <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'AG LAND',
  domain_desc = 'TOTAL',
  short_desc = 'AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / ACRE',
  variable_name = 'landValPerAcre'
)
get_str(results$assets_acre)

metas$assets <- tibble(
  dimension = "economics",
  index = 'food business profitability',
  indicator = 'assets and liabilities',
  metric = c(
    'Land and building value per farm',
    'Land and building value per acre'
  ),
  definition = c(
    'Value of agricultural land, including buildings, per operation',
    'Value of agricultural land, including buildings, per acre'
  ),
  variable_name = c(
    'landValPF',
    'landValPerAcre'
  ),
  axis_name = c(
    'Land Value Per Farm ($)',
    'Land Value Per Acre ($)'
  ),
  units = 'usd',
  scope = 'national',
  resolution = 'county, state',
  year = '2022',
  latest_year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation(access_date = '2025-01-07')



# Production --------------------------------------------------------------
## Production Inputs -------------------------------------------------------


# Chemical totals - $ and operations with expenses. Then calculate $/operation
results$chemical_total_expenses <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'CHEMICAL TOTALS',
  domain_desc = 'TOTAL',
  short_desc = 'CHEMICAL TOTALS - EXPENSE, MEASURED IN $',
  variable_name = 'expChemical'
)
get_str(results$chemical_total_expenses)

# As % of operating expenses
results$chemical_pct_of_expenses <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'CHEMICAL TOTALS',
  domain_desc = 'TOTAL',
  short_desc = 'CHEMICAL TOTALS - EXPENSE, MEASURED IN PCT OF OPERATING EXPENSES',
  variable_name = 'expChemicalPct'
)
get_str(results$chemical_pct_of_expenses)

# Number of operations
results$n_chemical_operations <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'CHEMICAL TOTALS',
  domain_desc = 'TOTAL',
  short_desc = 'CHEMICAL TOTALS - OPERATIONS WITH EXPENSE',
  variable_name = 'nOpsChemical'
)
get_str(results$n_chemical_operations)

# Metadata
metas$production_inputs <- tibble(
  dimension = "production",
  index = 'production margins',
  indicator = 'production inputs',
  metric = c(
    'Chemical expenses',
    'Number of operations with chemical expenses',
    'Mean chemical expenses per operation'
  ),
  variable_name = c(
    'expChemical',
    'nOpsChemical',
    'expChemicalPct'
  ),
  definition = c(
    'Total chemical expenses, measured in dollars',
    'Number of operations with chemical expenses',
    'Total chemical expenses / number of operations with chemical expenses'
  ),
  axis_name = c(
    'Chem expenses per farm ($)',
    'n farms with chem expenses',
    'Chem expenses per farm ($)'
  ),
  units = c(
    'usd',
    'count',
    'usd'
  ),
  scope = 'national',
  resolution = 'county, state',
  year = '2022',
  latest_year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation(access_date = access_date)



## Total Quantity Food Products --------------------------------------------


results$total_animal_sales <- pull_variable(
  coa_ne,
  sector_desc = 'ANIMALS & PRODUCTS',
  commodity_desc = 'ANIMAL TOTALS',
  domain_desc = 'TOTAL',
  short_desc = 'ANIMAL TOTALS, INCL PRODUCTS - SALES, MEASURED IN $',
  variable_name = 'salesAnimal'
)
get_str(results$total_animal_sales)

results$total_crop_sales <- pull_variable(
  coa_ne,
  sector_desc = 'CROPS',
  commodity_desc = 'CROP TOTALS',
  domain_desc = 'TOTAL',
  short_desc = 'CROP TOTALS - SALES, MEASURED IN $',
  variable_name = 'salesCrop'
)
get_str(results$total_crop_sales)

results$animal_sales_pct <- pull_variable(
  coa_ne,
  sector_desc = 'ANIMALS & PRODUCTS',
  commodity_desc = 'ANIMAL TOTALS',
  domain_desc = 'TOTAL',
  short_desc = 'ANIMAL TOTALS, INCL PRODUCTS - SALES, MEASURED IN PCT OF FARM SALES',
  variable_name = 'salesAnimalPctSales'
)
get_str(results$animal_sales_pct)

# # Get sum of animal and crop
# # Calculate expenses per operation
# total_animal_and_crop_sales <- bind_rows(
#   results$total_animal_sales, 
#   results$total_crop_sales
# ) %>% 
#   pivot_wider(
#     id_cols = 'fips',
#     names_from = 'variable_name', 
#     values_from = 'value'
#   ) %>%  
#   mutate(
#     salesAnimalCrop = salesAnimal + salesCrop,
#     .keep = 'unused'
#   )
# 
# # Remake the last one with it...
# results$total_animal_and_crop_sales <- results$total_crop_sales %>% 
#   full_join(total_animal_and_crop_sales, by = 'fips') %>% 
#   mutate(
#     value = salesAnimalCrop,
#     variable_name = 'salesAnimalCrop',
#     .keep = 'unused'
#   )
# get_str(results$total_animal_and_crop_sales)

# Metadata
metas$total_animal_and_crop_sales <- tibble(
  dimension = "production",
  index = 'production margins',
  indicator = 'total quantity food products',
  metric = c(
    'Total animal sales',
    'Total crop sales',
    'Animal sales as a percentage of farm sales'
  ),
  variable_name = c(
    'salesAnimal',
    'salesCrop',
    'salesAnimalPctSales'
  ),
  definition = c(
    'Total sales from animals and animal products',
    'Total sales from crops',
    'Animal sales as a percentage of total farm sales'
  ),
  axis_name = c(
    'Animal sales ($)',
    'Crop sales ($)',
    'Animal sales (%)'
  ),
  units = c(
    'usd',
    'usd',
    'percentage'
  ),
  scope = 'national',
  resolution = 'county, state',
  year = '2022',
  latest_year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation(access_date = '2025-01-07')



## Total Quantity Forest Products ------------------------------------------


results$total_forest_product_income <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'INCOME, FARM-RELATED',
  domain_desc = 'TOTAL',
  short_desc = 'INCOME, FARM-RELATED, FOREST PRODUCTS, (EXCL CHRISTMAS TREES & SHORT TERM WOODY CROPS & MAPLE SYRUP) - RECEIPTS, MEASURED IN $',
  variable_name = 'incForestProducts'
)
get_str(results$total_forest_product_income)

# Metadata
metas$total_forest_product_income <- tibble(
  dimension = "production",
  index = 'production margins',
  indicator = 'total quantity forest products',
  metric = c(
    'Total forest product income'
  ),
  variable_name = c(
    'incForestProducts'
  ),
  definition = c(
    'Income from farm-related forest products, exclusing Christmas trees, short term woody crops, and maple syrup'
  ),
  units = c(
    'usd'
  ),
  scope = 'national',
  resolution = 'county, state',
  year = '2022',
  latest_year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation(access_date = access_date)



# Social ------------------------------------------------------------------
## Food worker and farmer diversity ----------------------------------------


nass_vars <- c(
  'PRODUCERS - AGE, AVG, MEASURED IN YEARS',
  # 'PRODUCERS - AGE 25 TO 34 - NUMBER OF PRODUCERS',
  # 'PRODUCERS - AGE 35 TO 44 - NUMBER OF PRODUCERS',
  # 'PRODUCERS - AGE 45 TO 54 - NUMBER OF PRODUCERS',
  # 'PRODUCERS - AGE 55 TO 64 - NUMBER OF PRODUCERS',
  # 'PRODUCERS - AGE 65 TO 74 - NUMBER OF PRODUCERS',
  # 'PRODUCERS - AGE GE TO 75 - NUMBER OF PRODUCERS',
  'PRODUCERS - NUMBER OF PRODUCERS',
  'PRODUCERS, (ALL), FEMALE - NUMBER OF PRODUCERS',
  'PRODUCERS, (ALL), MALE - NUMBER OF PRODUCERS',
  'PRODUCERS, WHITE - NUMBER OF PRODUCERS',
  'PRODUCERS, AMERICAN INDIAN OR ALASKA NATIVE - NUMBER OF PRODUCERS',
  'PRODUCERS, ASIAN - NUMBER OF PRODUCERS',
  'PRODUCERS, BLACK OR AFRICAN AMERICAN - NUMBER OF PRODUCERS',
  'PRODUCERS, HISPANIC - NUMBER OF PRODUCERS',
  'PRODUCERS, NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER - NUMBER OF PRODUCERS'
)

variable_names <- c(
  'ageProducers',
  # 'n_producers_25_34',
  # 'n_producers_35_44',
  # 'n_producers_45_44',
  # 'n_producers_55_54',
  # 'n_producers_65_64',
  # 'n_producers_75_74',
  'nProducer',
  'nFemProducers',
  'nMaleProducers',
  'nWhiteProducers',
  'nAmInProducers',
  'nAsianProducers',
  'nBlackProducers',
  'nHispProducers',
  'nNaHaProducers'
)

social <- map2(nass_vars, variable_names, \(var, name) {
  pull_variable(
    coa_ne,
    sector_desc = 'DEMOGRAPHICS',
    commodity_desc = 'PRODUCERS',
    domain_desc = 'TOTAL',
    short_desc = var,
    variable_name = name
  )
}) %>% 
  setNames(c(variable_names))

get_str(social)



### Gender Ratio ------------------------------------------------------------


# Calculate ratio of female to male producers based on totals
names(social)
calc <- bind_rows(social$nMaleProducers, social$nFemProducers) %>% 
  pivot_wider(
    id_cols = c('fips', 'year'),
    names_from = 'variable_name',
    values_from = 'value'
  ) %>% 
  mutate(
    ftmProdRatio = nFemProducers / nMaleProducers,
    .keep = 'unused'
  ) %>% 
  pivot_longer(
    cols = 'ftmProdRatio',
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(calc)

social$female_to_male_producer_ratio <- calc
get_str(social)

results$social <- bind_rows(social)
get_str(results$social)



## Metadata ---------------------------------------------------------------


get_str(results$social)
(vars <- get_vars(results$social))

metas$social <- tibble(
  dimension = "social",
  index = 'food and farmworker diversity',
  indicator = c(
    'age diversity',
    'gender diversity',
    rep('racial diversity', 3),
    'gender diversity',
    'racial diversity',
    'gender diversity',
    'racial diversity',
    'racial diversity',
    'racial diversity'
  ),
  metric = c(
    'Mean producer age',
    'Ratio of female to male producers',
    'Number of American Indian producers',
    'Number of Asian producers',
    'Number of Black producers',
    'Number of female producers',
    'Number of Hispanic producers',
    'Number of male producers',
    'Number of Native Hawaiian or Pacific Islander producers',
    'Total number of producers',
    'Number of White producers'
  ),
  definition = metric,
  variable_name = vars,
  units = c(
    'ratio',
    'age',
    rep('count', 9)
  ),
  scope = 'national',
  resolution = 'county, state',
  year = get_all_years(results$social),
  latest_year = get_max_year(results$social),
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation(access_date = '2024-11-13')



# Environment -------------------------------------------------------------

# New list for env variables
env <- list()


## Conservation Income -----------------------------------------------------


# Note to filter by value or value codes - D is missing data, and there is a lot
vars <- c(
  'GOVT PROGRAMS, FEDERAL, CONSERVATION & WETLANDS - OPERATIONS WITH RECEIPTS',
  'GOVT PROGRAMS, FEDERAL, CONSERVATION & WETLANDS - RECEIPTS, MEASURED IN $',
  'GOVT PROGRAMS, FEDERAL, CONSERVATION & WETLANDS - RECEIPTS, MEASURED IN $ / OPERATION'
)

names <- c(
  'consIncomeNOps',
  'consIncomeTotal',
  'consIncomePF'
)

env$cons <- map2(vars, names, \(var, name) {
  print(paste(var, '/', name))
  out <- pull_variable(
    coa_ne,
    sector_desc = 'ECONOMICS',
    commodity_desc = 'GOVT PROGRAMS',
    domain_desc = 'TOTAL',
    short_desc = var,
    variable_name = name
  ) %>% 
    filter(!is.na(value))
  return(out)
})
get_str(env$cons)



## Practices ---------------------------------------------------------------


# Census > Economics > Practices
vars <- c(
  'PRACTICES, ALLEY CROPPING & SILVAPASTURE - NUMBER OF OPERATIONS',
  
  'PRACTICES, LAND USE, CONSERVATION EASEMENT - ACRES',
  'PRACTICES, LAND USE, CONSERVATION EASEMENT - AREA, MEASURED IN ACRES / OPERATION',
  'PRACTICES, LAND USE, CONSERVATION EASEMENT - NUMBER OF OPERATIONS',
  
  'PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES',
  'PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - AREA, MEASURED IN ACRES / OPERATION',
  'PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - NUMBER OF OPERATIONS',
  
  'PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES',
  'PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - AREA, MEASURED IN ACRES / OPERATION',
  'PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - NUMBER OF OPERATIONS',
  
  'PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - ACRES',
  'PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - AREA, MEASURED IN ACRES / OPERATION',
  'PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - NUMBER OF OPERATIONS',
  
  'PRACTICES, LAND USE, DRAINED BY ARTIFICIAL DITCHES - ACRES',
  'PRACTICES, LAND USE, DRAINED BY ARTIFICIAL DITCHES - AREA, MEASURED IN ACRES / OPERATION',
  'PRACTICES, LAND USE, DRAINED BY ARTIFICIAL DITCHES - NUMBER OF OPERATIONS',
  
  'PRACTICES, LAND USE, DRAINED BY TILE - ACRES',
  'PRACTICES, LAND USE, DRAINED BY TILE - AREA, MEASURED IN ACRES / OPERATION',
  'PRACTICES, LAND USE, DRAINED BY TILE - NUMBER OF OPERATIONS',
  
  'PRACTICES, PRECISION AGRICULTURE - NUMBER OF OPERATIONS',
  'PRACTICES, ROTATIONAL OR MGMT INTENSIVE GRAZING - NUMBER OF OPERATIONS'
)

names <- c(
  'alleyCropSilvapastureNOps',
  
  'consEasementAcres',
  'consEasementAcresPF',
  'consEasementNOps',
  
  'consTillExclNoTillAcres',
  'consTillExclNoTillAcresPF',
  'consTillExclNoTillNOps',
  
  'consTillNoTillAcres',
  'consTillNoTillAcresPF',
  'consTillNoTillNOps',
  
  'coverCropExclCrpAcres',
  'coverCropExclCrpAcresPF',
  'coverCropExclCrpNOps',
  
  'drainedDitchesAcres',
  'drainedDitchesAcresPF',
  'drainedDitchesNOps',
  
  'drainedTileAcres',
  'drainedTileAcresPF',
  'drainedTileNOps',
  
  'precisionAgNOps',
  'rotateIntenseGrazeNOps'
)

# Pull all these variables  
env$practices <- map2(vars, names, \(var, name) {
  print(paste(var, '/', name))
  out <- pull_variable(
    coa_ne,
    sector_desc = 'ECONOMICS',
    commodity_desc = 'PRACTICES',
    domain_desc = 'TOTAL',
    short_desc = var,
    variable_name = name
  )
  return(out)
}) %>% 
  setNames(c(names))
get_str(env$practices)



## Fertilizer --------------------------------------------------------------


vars <- c(
  'FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $',
  'FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN PCT OF OPERATING EXPENSES',
  'FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - OPERATIONS WITH EXPENSE'
)

names <- c(
  'fertExpenseTotal',
  'fertExpensePercOpExp',
  'fertExpenseOpsWithExp'
)

# Pull all these variables  
env$fert <- map2(vars, names, \(var, name) {
  print(paste(var, '/', name))
  out <- pull_variable(
    coa_ne,
    sector_desc = 'ECONOMICS',
    commodity_desc = 'FERTILIZER TOTALS',
    domain_desc = 'TOTAL',
    short_desc = var,
    variable_name = name
  )
  return(out)
}) %>% 
  setNames(c(names))
get_str(env$fert)



## Renewable Energy --------------------------------------------------------


# These are all at state level
# vars <- c(
#   'ENERGY, RENEWABLE - NUMBER OF OPERATIONS',
#   'ENERGY, RENEWABLE, GEOEXCHANGE SYSTEMS - NUMBER OF OPERATIONS',
#   'ENERGY, RENEWABLE, WIND RIGHTS, RENTED TO OTHERS - NUMBER OF OPERATIONS',
#   'ENERGY, RENEWABLE, SOLAR PANELS - OPERATIONS WITH DEVICES',
#   'ENERGY, RENEWABLE, WIND TURBINES - OPERATIONS WITH DEVICES',
#   'ENERGY, RENEWABLE, METHANE DIGESTERS - OPERATIONS WITH DEVICES'
# )

vars <- coa_ne %>% 
  filter(str_detect(short_desc, 'ENERGY, RENEWABLE')) %>% 
  select(short_desc) %>% 
  unique() %>% 
  pull(short_desc) %>% 
  sort
vars

names <- c(
  'renewableNOps',
  'biodieselNOps',
  'ethanolNOps',
  'geoexchangeNOps',
  'biomassHarvestNOps',
  'methaneNOps',
  'otherRenewableNOps',
  'smallHydroNOps',
  'solarNOps',
  'windRentedToOthersNOps',
  'windTurbinesNOps'
)

env$renew <- map2(vars, names, \(var, name) {
  print(paste(var, '/', name))
  out <- pull_variable(
    coa_ne,
    sector_desc = 'ECONOMICS',
    commodity_desc = 'ENERGY',
    domain_desc = 'TOTAL',
    short_desc = var,
    variable_name = name
  ) %>% 
    select(fips, year, variable_name, value)
  return(out)
}) %>% 
  setNames(c(names))
get_str(env$renew)



# Water -------------------------------------------------------------------


# Pulling from 2023 cycle datasets
# water <- readRDS('1_raw/nass/nass_states_irrigation_2013-2023_5yr.rds')
water <- read_csv('1_raw/nass/census_water_addendum_2013_2018_2023.csv') %>% 
  janitor::clean_names()
get_str(water)
water$data_item %>% unique

var_crosswalk <- data.frame(
  data_item = c(
    'WATER, IRRIGATION, RECLAIMED - WATER APPLIED, MEASURED IN ACRE FEET',
    'WATER, IRRIGATION, RECLAIMED, IN THE OPEN - ACRES IRRIGATED',
    'WATER, IRRIGATION, RECLAIMED, IN THE OPEN - OPERATIONS WITH AREA IRRIGATED',
  
    'WATER, IRRIGATION, SOURCE = OFF FARM - EXPENSE, MEASURED IN $',
    'WATER, IRRIGATION, SOURCE = OFF FARM - EXPENSE, MEASURED IN $ / ACRE FOOT',
    'WATER, IRRIGATION, SOURCE = OFF FARM - EXPENSE, MEASURED IN $ / ACRE IRRIGATED',
    'WATER, IRRIGATION, SOURCE = OFF FARM - OPERATIONS WITH EXPENSE'
    ),
  variable_name = c(
    'waterIrrReclaimedAcreFt',
    'waterIrrReclaimedOpenAcres',
    'waterIrrReclaimedOpenAreaNOps',
    'waterIrrSrcOffFarmExp',
    'waterIrrSrcOffFarmExpPerAcreFt',
    'waterIrrSrcOffFarmExpPerAcre',
    'waterIrrSrcOffFarmNOpsWithExp'
  )
)
var_crosswalk$data_item %in% water$data_item

### We are right here []. Have to wrangle slightly different dataset here.
get_str(water)
water_df <- water %>% 
  select(
    fips = state_ansi, 
    year, 
    data_item, 
    value
  ) %>% 
  inner_join(var_crosswalk) %>% 
  select(-data_item)
get_str(water_df)  

# Save to results
env$water <- water_df



# ## Wells -------------------------------------------------------------------
# 
# 
# vars <- c(
#   'WELLS, USED FOR IRRIGATION - NUMBER OF WELLS',
#   'WELLS, USED FOR IRRIGATION - OPERATIONS WITH WELLS',
#   'WELLS, USED FOR IRRIGATION, PUMPED - WELL DEPTH, MEASURED IN FEET'
# )
# 
# names <- c(
#   'wellsIrrigationN',
#   'wellsIrrigationNOps',
#   'wellsIrrigationDepthFt'
# )
# 
# 
# env$wells <- map2(vars, names, \(var, name) {
#   pull_variable(
#     water,
#     sector_desc = 'ECONOMICS',
#     commodity_desc = 'WELLS',
#     domain_desc = 'TOTAL',
#     short_desc = var,
#     variable_name = name
#   ) %>% 
#     filter(
#       !is.na(value),
#       !str_detect(value, '(D)')
#     )
# })
# get_str(env$wells)



## Combine Env -------------------------------------------------------------


get_str(env)
test <- list(flatten(env[setdiff(names(env), 'water')]), env$water)
get_str(test)

# Remove water because it is already good to go
no_water <- env[!names(env) %in% 'water']
get_str(no_water)

env_df <- flatten(no_water) %>% 
  map(\(x) mutate(x, value = as.character(value))) %>% 
  bind_rows() %>% 
  bind_rows(env$water) %>% 
  select(fips, year, variable_name, value, unit_desc)
get_str(env_df)

# Add to results
results$env <- env_df



## Metadata ----------------------------------------------------------------


get_str(env_df)
(vars <- get_vars(env_df))

starter <- env_df %>% 
  group_by(variable_name) %>% 
  summarize(
    units = unit_desc %>% 
      unique() %>% 
      str_to_lower() %>% 
      str_replace_all('\\$', 'usd') %>% 
      str_replace_all('operations', 'count'),
    latest_year = max(year),
    year = paste0(unique(year), collapse = ', '),
    fips_length = min(str_length(fips))
  ) %>% 
  mutate(
    units = case_when(
      str_detect(variable_name, 'NOps$') ~ 'count',
      str_detect(variable_name, 'dAcreFt$') ~ 'acre feet',
      str_detect(variable_name, 'Exp$') ~ 'usd',
      str_detect(variable_name, 'ExpPerAcre$') ~ 'usd per acre',
      str_detect(variable_name, 'OpenAcres$') ~ 'acres',
      str_detect(variable_name, 'ExpPerAcreFt$') ~ 'usd per acre foot',
      .default = units
    )
  )
get_str(starter)
(vars <- get_vars(env_df))

# Environment metadata
metas$env <- starter %>% 
  mutate(
    dimension = "environment",
    metric = c(
      'Operations practicing alley cropping or silvapasture',
      'Number of operations with biodiesel energy',
      'Number of operations harvesting biomass for energy',
      'Acres under conservation easements',
      'Acres under conservation easement per operation',
      
      'Operations with conservation easements',
      'Operations with conservation income',
      'Conservation income per operation',
      'Total conservation income',
      'Conservation tillage acres, excluding no-till',
      # 10 
      'Conservation tillage acres, excluding no-till, per operation',
      'Operations with conservation tillage, excluding no-till',
      'Conservation tillage, no-till acres',
      'Conservation tillage, no-till acres per operation',
      'Conservation tillage, no-till, operations',
      # 15
      'Cover crop practices, excluding CRP, acres',
      'Cover crop practices, excluding CRP, acres per operation',
      'Cover crop practices, excluding CRP, operations',
      'Acres drained by ditches',
      'Acres drained by ditches per farm',
      # 20
      'Operations with drained acres by tile',
      'Acres drained by tile',
      'Acres drained by tile per operation',
      'Operations with acres drained by tile',
      'Operations using ethanol',
      # 25
      'Operations with fertilizer expenses',
      'Fertilizer expenses as a percentage of operations expenses',
      'Total fertilizer expenses',
      'Operations using geoexchange',
      'Operations using methane energy',
      # 30
      'Operations using other renewable energy',
      'Operations using precision agriculture',
      'Operations using any renewable energy',
      'Operations using rotational or intensive grazing',
      'Operations using small hydro energy',
      # 35
      'Operations with solar power',
      'Acre feet of irrigation from reclaimed water',
      'Acres irrigated with reclaimed water in open areas',
      'Operations irrigating with reclaimed water in open areas',
      'Total expenses from water for irrigation sourced off-farm',
      # 40
      'Expenses from water for irrigation sourced off-farm per acre',
      'Expenses from water for irrigation sourced off-farm per acre foot',
      'Operations with expenses from water for irrigation sourced off-farm',
      # 'Depth of irrigation wells',
      # 'Number of irrigation wells',
      # 'Operations with irrigation wells',
      'Operations with wind turbines rented to others',
      'Operations with wind turbines'
      # 45
    ),
    indicator = case_when(
      str_detect(metric, 'solar|wind|methane|geo') ~ 'energy efficiency',
      str_detect(
        metric,
        regex('water|well|irrigation|drain|ditch', ignore_case = TRUE)
      ) ~ 'water use / irrigation efficiency',
      .default = 'acres in conservation practices'
    ), 
    index = case_when(
      indicator == 'acres in conservation practices' ~ 'biodiversity',
      str_detect(indicator, 'efficien') ~ 'resource use efficiency'
    ),
    definition = metric, # Fix this at some point
    axis_name = variable_name, # Fix this at some point
    units = case_when(
      str_detect(variable_name, 'NOps') ~ 'count',
      str_detect(variable_name, '^pct') ~ 'percentage',
      str_detect(variable_name, 'AcreFt$') ~ 'acre feet',
      str_detect(variable_name, 'Acres') ~ 'acres',
      str_detect(variable_name, 'DepthFt$') ~ 'feet',
      str_detect(variable_name, 'Exp$') ~ 'usd',
      str_detect(variable_name, 'ExpPerAcre$') ~ 'usd per acre',
      .default = units
    ),
    scope = 'national',
    resolution = case_when(
      fips_length == 5 ~ 'county',
      fips_length == 2 ~ 'state',
      .default = NA_character_
    ),
    updates = "5 years",
    source = paste0(
      "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
      "(2024). 2022 Census of Agriculture."
    ),
    url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/',
    warehouse = FALSE
  ) %>%
  add_citation(access_date = access_date) %>% 
  select(-fips_length)

metas$env


# Aggregate ---------------------------------------------------------------


# Put metrics and metadata together into two single DFs
out <- aggregate_metrics(results, metas)

# Check record counts
check_n_records(out$result, out$meta, 'nass')

saveRDS(out$result, '5_objects/metrics/nass.RDS')
saveRDS(out$meta, '5_objects/metadata/nass_meta.RDS')

clear_data()
gc()
