#' Labor Costs


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  readr,
  purrr,
  janitor,
  skimr,
  tidyr,
  sf
)

# Source functions
source('3_functions/pull_variable.R')
source('3_functions/add_citation.R')
source('3_functions/check_n_records.R')

# Pull Census of Ag data filtered to New England
coa_ne <- readRDS('5_objects/coa_ne.rds')

# Pull fips key for NE counties and states
# fips_key <- readRDS('5_objects/fips_key.rds')

# Initialize results lists
results <- list()
metas <- list()



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
    value,
    value_codes
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
    cols = !c(fips, county_name, state_name, year, value_codes),
    names_to = 'variable_name',
    values_to = 'value'
  ) %>% 
  select(
    fips,
    county_name,
    state_name,
    year,
    variable_name,
    value,
    value_codes
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
  resolution = 'county',
  year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation()

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
    value,
    value_codes,
    cv_percent
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
  resolution = 'county',
  year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation()



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
    "mean farm related income per operation"
  ), 
  variable_name = c(
    "farmIncomePF"
  ),
  axis_name = 'Income per farm ($)',
  definition = 'Income, farm-related - receipts, measured in $ / operation',
  units = 'usd',
  scope = 'national',
  resolution = 'county',
  year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation()



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
  resolution = 'county',
  year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation()



## Employee Numbers --------------------------------------------------------


# results$employee_numbers <- pull_variable(
#   coa_ne,
#   sector_desc = 'ECONOMICS',
#   commodity_desc = 'FARM OPERATIONS',
#   domain_desc = 'TOTAL',
#   short_desc = 'FARM OPERATIONS - ACRES OPERATED',
#   variable_name = 'total_acres_operated'
# ) %>% 
#   mutate(
#     dimension = 'economics',
#     index = 'food business profitability',
#     indicator = 'acreage in production',
#     metric = 'Total acres operated',
#     resolution = 'county'
#   )
# 


## Operator salary ---------------------------------------------------------


# results$operator_salary <- pull_variable(
#   coa_ne,
#   sector_desc = 'ECONOMICS',
#   commodity_desc = 'FARM OPERATIONS',
#   domain_desc = 'TOTAL',
#   short_desc = 'FARM OPERATIONS - ACRES OPERATED',
#   variable_name = 'total_acres_operated'
# ) %>%
#   mutate(
#     dimension = 'economics',
#     index = 'food business profitability',
#     indicator = 'acreage in production'
#   )
# 
# get_str(results$operator_salary)
# results$operator_salary$variable_name %>% unique



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
  resolution = 'county',
  year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation()



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
get_str(results$chemical_total_dollars)

# Number of operations
results$n_chemical_operations <- pull_variable(
  coa_ne,
  sector_desc = 'ECONOMICS',
  commodity_desc = 'CHEMICAL TOTALS',
  domain_desc = 'TOTAL',
  short_desc = 'CHEMICAL TOTALS - OPERATIONS WITH EXPENSE',
  variable_name = 'nOpsChemical'
)
get_str(results$chemical_operations)

# Calculate expenses per operation
chemical_expenses_per_operation <- bind_rows(
  results$n_chemical_operations, 
  results$chemical_total_expenses
) %>% 
  pivot_wider(
    id_cols = 'fips',
    names_from = 'variable_name', 
    values_from = 'value'
  ) %>%  
  mutate(
    expChemPF = expChemical / nOpsChemical,
    .keep = 'unused'
  )

# Remake the last one with it...
results$chemical_expenses_per_operation <- results$n_chemical_operations %>% 
  full_join(chemical_expenses_per_operation, by = 'fips') %>% 
  mutate(
    value = expChemPF,
    variable_name = 'expChemPF',
    .keep = 'unused'
  )

get_str(results$chemical_expenses_per_operation)

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
    'expChemPF'
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
  resolution = 'county',
  year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation()



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

# Get sum of animal and crop
# Calculate expenses per operation
total_animal_and_crop_sales <- bind_rows(
  results$total_animal_sales, 
  results$total_crop_sales
) %>% 
  pivot_wider(
    id_cols = 'fips',
    names_from = 'variable_name', 
    values_from = 'value'
  ) %>%  
  mutate(
    salesAnimalCrop = salesAnimal + salesCrop,
    .keep = 'unused'
  )

# Remake the last one with it...
results$total_animal_and_crop_sales <- results$total_crop_sales %>% 
  full_join(total_animal_and_crop_sales, by = 'fips') %>% 
  mutate(
    value = salesAnimalCrop,
    variable_name = 'salesAnimalCrop',
    .keep = 'unused'
  )
get_str(results$total_animal_and_crop_sales)

# Metadata
metas$total_animal_and_crop_sales <- tibble(
  dimension = "production",
  index = 'production margins',
  indicator = 'total quantity food products',
  metric = c(
    'Total animal sales',
    'Total crop sales',
    'Total animal and crop sales'
  ),
  variable_name = c(
    'salesAnimal',
    'salesCrop',
    'salesAnimalCrop'
  ),
  definition = c(
    'Total sales from animals and animal products',
    'Total sales from crops',
    'Sum of total sales from animals, animal products, and crops'
  ),
  axis_name = c(
    'Animal sales ($)',
    'Crop sales ($)',
    'Animal, crop sales ($)'
  ),
  units = c(
    'usd',
    'usd',
    'usd'
  ),
  scope = 'national',
  resolution = 'county',
  year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation()



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
  resolution = 'county',
  year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation()



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
    id_cols = 'fips',
    names_from = 'variable_name',
    values_from = 'value'
  ) %>% 
  mutate(
    ftmProdRatio = nFemProducers / nMaleProducers,
    .keep = 'unused'
  )
get_str(calc)

# Doing some weird shit here. Joining it back to a random table to get the 
# geographic variables back only. This is an awkward section. Worth updating
# at some point.
calc <- calc %>%
  full_join(social$ageProducers, by = 'fips') %>%
  mutate(
    value = ftmProdRatio,
    variable_name = 'ftmProdRatio',
    .keep = 'unused'
  )
get_str(calc)

social$female_to_male_producer_ratio <- calc
get_str(social)

results$social <- bind_rows(social)
get_str(results$social)



### Race ratio --------------------------------------------------------------


# NOTE: I don't trust these numbers enough to make a ratio of bipoc to white.
# Some of them come out negative - not a good sign


# Calculate ratio bipoc to white producers
# names(diversity)
# race <- bind_rows(
#   diversity[
#     str_detect(
#       names(diversity), 
#       'white|indian|asian|black|hispanic|native'
#     )
#   ]
# ) %>% 
#   pivot_wider(
#     id_cols = 'fips',
#     names_from = 'variable_name',
#     values_from = 'value'
#   ) %>% 
#   mutate(
#     bipoc_white_ratio = 1 - (n_white_producers / sum(
#       n_american_indian_producers,
#       n_asian_producers,
#       n_black_producers,
#       n_hispanic_producers,
#       n_native_hawaiian_producers,
#       na.rm = TRUE
#     )),
#     .keep = 'unused'
#   )
# get_str(race)
# 
# results$race <- diversity$mean_producer_age %>% 
#   full_join(race, by = 'fips') %>% 
#   mutate(
#     value = bipoc_white_ratio,
#     variable_name = 'bipoc_white_ratio',
#     .keep = 'unused'
#   )
# get_str(results$race)



## Social Metadata ---------------------------------------------------------


get_str(results$social)
vars <- results$social$variable_name %>% 
  unique %>% 
  sort
vars

metas$social <- tibble(
  dimension = "social",
  index = 'food and farmworker diversity',
  indicator = c(
    'gender diversity',
    'age diversity',
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
  resolution = 'county',
  year = '2022',
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation()



# Aggregate ---------------------------------------------------------------


get_str(results)
map(results, get_str)
result <- bind_rows(results) %>% 
  select(fips, year, variable_name, value, value_codes, cv_percent)
get_str(result)

meta <- bind_rows(metas)
get_str(meta)
head(meta)

# Check records to make sure they are the same, meta and metrics
check_n_records(result, meta, 'NASS')

saveRDS(result, '5_objects/metrics/nass.RDS')
saveRDS(meta, '5_objects/metadata/nass_meta.RDS')

clear_data()
