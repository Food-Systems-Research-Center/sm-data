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

# Pull Census of Ag data filtered to New England
coa_ne <- readRDS('5_objects/coa_ne.rds')

# county fips for New England (differences for CT restructuring)
fips_2021 <- readRDS('5_objects/fips_2021.rds')
fips_2024 <- readRDS('5_objects/fips_2024.rds')
fips_all <- readRDS('5_objects/fips_all.rds')

# Initialize results lists
results <- list()
metas <- list()



# Economics ---------------------------------------------------------------
## Labor Total -------------------------------------------------------------


results$labor_costs <- coa_ne %>% 
  filter(
    commodity_desc == 'LABOR',
    domain_desc == 'TOTAL'
  ) %>% 
  
  # New column with the variables we want
  mutate(
    variable_name = case_when(
      short_desc == 'LABOR, HIRED - EXPENSE, MEASURED IN $' ~ 'expHiredLabor',
      short_desc == 'LABOR, HIRED - OPERATIONS WITH EXPENSE' ~ 'nOpsHiredLabor'
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
  ) %>% 
  filter(!is.na(variable_name)) %>% 
  
  # Pivot to get average by operation, then put it back to long format
  pivot_wider(
    names_from = 'variable_name'
  ) %>% 
  mutate(expHiredLaborPF = expHiredLabor / nOpsHiredLabor) %>% 
  pivot_longer(
    cols = nOpsHiredLabor:last_col(),
    names_to = 'variable_name'
  ) %>% 
  select(
    fips,
    county_name,
    state_name,
    year,
    variable_name,
    value,
    value_codes
  )

# Write metadata for farm labor
metas$labor_costs <- data.frame(
  dimension = "economics",
  index = 'food business profitability',
  indicator = 'labor costs',
  metric = c(
    "Labor expenses",
    "Number of farms with hired labor",
    "Labor expenses per farm"
  ), 
  variable_name = c(
    "expHiredLabor",
    "nOpsHiredLabor",
    "expHiredLaborPF"
  ),
  definition = c(
    "Total expenses from hired labor",
    "Number of operations with hired labor expenses",
    'Expenses of hired labor per farm (nOpsHiredLabor/expHiredLabor)'
  ),
  axis_name = c(
    "Labor expenses ($)",
    "n farms with hired labor",
    "Labor expense per farm ($)"
  ),
  units = c('usd', 'count', 'usd'),
  scope = 'national',
  resolution = 'county',
  year = '2022',
  all_years = "2022",
  updates = "5 years",
  source = paste0(
    "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
    "(2024). 2022 Census of Agriculture."
  ),
  url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/'
) %>% 
  add_citation()



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
    value_codes
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
  all_years = '2022',
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
  all_years = '2022|2017|2012|2007|2002',
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
  all_years = '2022|2017|2012|2007|2002',
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

# 
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
# get_str(results$acres_operated)



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
  all_years = '2022|2017|2012|2007|2002',
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
  all_years = '2022|2017|2012|2007|2002',
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
  all_years = '2022|2017|2012|2007|2002',
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
    'Ratio of female to male producers',
    'Mean producer age',
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
  all_years = '2022|2017|2012|2007|2002',
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
result <- bind_rows(results) %>% 
  select(fips, year, variable_name, value, value_codes)
get_str(result)

meta <- bind_rows(metas)
get_str(meta)
head(meta)

saveRDS(result, '5_objects/metrics/nass.RDS')
saveRDS(meta, '5_objects/metadata/nass_meta.RDS')
