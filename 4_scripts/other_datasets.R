# EPA GHG data
# 2024-09-10



# Housekeeping -------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr,
  tidyr,
  snakecase,
  readxl
)

source('3_functions/aggregate_metrics.R')
source('3_functions/add_citation.R')
source('3_functions/check_n_records.R')
source('3_functions/filter_fips.R')
fips_key <- readRDS('5_objects/fips_key.rds')

results <- list()
metas <- list()



# EPA GHGs from Ag --------------------------------------------------------


# https://cfpub.epa.gov/ghgdata/inventoryexplorer/
# Read in files from inventory, add _inv to variable name to keep them straight
inv_paths <- list.files(
  '1_raw/epa/ghg_ide_agriculture_inventory/',
  full.names = TRUE
)
names <- map(inv_paths, ~ names(read_csv(.x))[1])

inv <- map(inv_paths, ~ {
  df <- read_csv(.x)
  var_name <- names(df)[1]
  df %>% 
    rename('state' = 1) %>% 
    filter(state %in% fips_key$state_name) %>% 
    pivot_longer(
      cols = !state,
      names_to = 'year',
      values_to = 'value'
    ) %>% 
    mutate(
      metric = paste(
        str_replace(str_split_i(var_name, ', ', 2), '\\.', ','),
        str_split_i(var_name, ',', 1),
        '(Inventory Sectors)'
      ),
      variable_name = paste0(
        'mmtCo2',
        var_name %>%
          str_split_i(',', 1) %>%
          to_lower_camel_case(),
        'Inv'
      )
    )
})
get_str(inv[[1]])
get_str(inv)

# Same for economic sectors
econ_paths <- list.files(
  '1_raw/epa/ghg_ide_agriculture_economic/',
  full.names = TRUE
)
econ <- map(econ_paths, ~ {
  df <- read_csv(.x)
  var_name <- names(df)[1]
  df %>% 
    rename('state' = 1) %>% 
    filter(state %in% fips_key$state_name) %>% 
    pivot_longer(
      cols = !state,
      names_to = 'year',
      values_to = 'value'
    ) %>% 
    mutate(
      metric = paste(
        str_replace(str_split_i(var_name, ', ', 2), '\\.', ','),
        str_split_i(var_name, ',', 1),
        '(Economic Sectors)'
      ),
      variable_name = paste0(
        'mmtCo2',
        var_name %>%
          str_split_i(',', 1) %>%
          to_lower_camel_case(),
        'Econ'
      )
    )
})
get_str(econ)
econ[[1]]

# Combine and bring fips
state_key <- fips_key %>% 
  select(fips, state_name) %>% 
  filter(str_length(fips) == 2, state_name != 'US')
dat <- c(econ, inv) %>% 
  bind_rows() %>% 
  left_join(state_key, by = join_by(state == state_name)) %>% 
  select(-state)
get_str(dat)

# Check vars
vars <- dat$variable_name %>% 
  unique %>% 
  sort
vars

# Add to results
results$ghgs <- dat



## Metadata ----------------------------------------------------------------


# Check vars
vars
get_str(results$ghgs)

metas$ghgs <- results$ghgs %>% 
  select(metric, variable_name) %>% 
  unique() %>% 
  mutate(
    dimension = "environment",
    index = 'air quality',
    indicator = 'greenhouse gas emissions',
    definition = paste(
      'Greenhouse gas emissions by state in millions of metric tons.',
      'Categorized as either economic sector or inventory sector, the latter of which is consistent with international standards.'
    ),
    units = 'mmt CO2 equivalent',
    axis_name = variable_name,
    scope = 'national',
    resolution = 'state',
    year = paste(sort(unique(results$ghgs$year)), collapse = ', '),
    latest_year = max(unique(results$ghgs$year)),
    updates = "annual",
    source = paste0(
      'U.S. Environmental Protection Agency, Greenhouse Gas Inventory Explorer, 2023'
    ),
    url = 'https://cfpub.epa.gov/ghgdata/inventoryexplorer/'
  ) %>% 
  add_citation()
metas$ghgs



# EPA NARS ----------------------------------------------------------------
## Lakes -------------------------------------------------------------------


## Load
lakes <- read_csv('1_raw/epa/nars/lakes_data_for_population_estimates_2022.csv')

lakes_meta <- read.table(
  '1_raw/epa/nars/lakes_metadata.txt',
  header = TRUE, 
  sep = '\t',
  fill = TRUE
) %>% 
  filter()

lakes_meta <- read_tsv(
  '1_raw/epa/nars/lakes_metadata.txt',
  col_names = TRUE
)


## Explore
get_str(lakes)
get_str(lakes_meta)
lakes_meta %>% 
  filter(str_detect(COLUMN_NAME, 'COND$')) %>% 
  select(COLUMN_NAME, LEGAL_VALUES)


## Clean
lake_dat <- lakes %>% 
  select(
    PSTL_CODE,
    ends_with('COND'),
    -DRAWDOWN_COND
  ) %>% 
  mutate(
    across(matches('^ACID|^CHLA|^LITCVR|^LITRIPCVR|^NTL|^PTL|^RDIS|^RVEG'), ~ case_when(
      .x %in% c('Good', 'Fair') ~ 1,
      .x %in% c('Poor', 'Not Assessed') ~ 0,
      .default = NA
    )),
    across(matches('^CYLSPER|^ENT_|MICX_'), ~ case_when(
      .x == 'Above Benchmark' ~ 1,
      .x == 'At or Below Benchmark' ~ 0,
      .default = NA
    ))
  ) %>% 
  group_by(PSTL_CODE) %>% 
  summarize(across(matches('COND$'), ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(
    cols = ends_with('COND'),
    names_to = 'variable_name',
    values_to = 'value'
  ) %>% 
  inner_join(fips_key, by = join_by(PSTL_CODE == state_code)) %>% 
  select(fips, variable_name, value) %>% 
  mutate(year = '2022') %>% 
  filter(variable_name != 'RDIS_COND')
get_str(lake_dat)
lake_dat

results$lakes <- lake_dat



### Metadata ----------------------------------------------------------------


get_str(results$lakes)


metas$lakes <- lakes_meta %>%
  filter(COLUMN_NAME %in% unique(results$lakes$variable_name)) %>% 
  rename(variable_name = COLUMN_NAME, metric = LABEL) %>% 
  mutate(
    variable_name = paste0('lakes', snakecase::to_upper_camel_case(variable_name)),
    metric = case_when(
      str_detect(metric, '^Enterococci') ~ str_split_i(metric, ',', 1),
      .default = metric
    ), 
    definition = case_when(
      str_detect(LEGAL_VALUES, 'Benchmark') ~ 'Qualitative ratings (Above Benchmark, At or Below Benchmark) recoded to (1, 0) and averaged by state',
      .default = 'Qualitative ratings recoded to 1 (Good, Fair) and 0 (Poor) and averaged by state'
    ),
    dimension = "environment",
    axis_name = variable_name,
    index = 'water quality',
    indicator = 'quantity / quality of surface and shallow subsurface water',
    units = 'proportion',
    scope = 'national',
    resolution = 'state',
    year = '2022',
    latest_year = '2022',
    updates = "5 years",
    source = 'U.S. Environmental Protection Agency, National Aquatic Resource Surveys, 2022',
    url = 'https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys'
  ) %>% 
  add_citation() %>% 
  select(-LEGAL_VALUES)

metas$lakes  

# Go back and fix variable names from dataset
results$lakes <- results$lakes %>% 
  mutate(variable_name = paste0('lakes', snakecase::to_upper_camel_case(variable_name)))



## Rivers ------------------------------------------------------------------


rivers <- read_csv('1_raw/epa/nars/rivers_data_for_population_estimates_2019.csv')
get_str(rivers)

rivers_meta <- read.table(
  '1_raw/epa/nars/rivers_metadata.txt',
  header = TRUE, 
  sep = '\t',
  fill = TRUE
)
get_str(rivers_meta)
river_vars <- rivers_meta %>%
  filter(str_detect(COLUMN_NAME, 'COND$')) %>% 
  select(COLUMN_NAME, LEGAL_VALUES) %>% 
  filter(str_detect(LEGAL_VALUES, 'Good')) %>% 
  pull(COLUMN_NAME)
river_vars

river_dat <- rivers %>% 
  inner_join(fips_key, by = join_by(PSTL_CODE == state_code)) %>% 
  select(fips, all_of(river_vars)) %>% 
  group_by(fips) %>% 
  summarize(across(everything(), ~ mean(.x %in% c('Good', 'Fair')))) %>% 
  pivot_longer(
    cols = !fips,
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  mutate(year = '2019')
get_str(river_dat)

results$rivers <- river_dat



### Metadata ----------------------------------------------------------------


get_str(results$rivers)


metas$rivers <- rivers_meta %>%
  filter(COLUMN_NAME %in% unique(results$rivers$variable_name)) %>% 
  rename(variable_name = COLUMN_NAME, metric = LABEL) %>% 
  mutate(
    variable_name = paste0('rivers', snakecase::to_upper_camel_case(variable_name)),
    definition = case_when(
      str_detect(LEGAL_VALUES, 'Benchmark') ~ 'Qualitative ratings (Above Benchmark, At or Below Benchmark) recoded to (1, 0) and averaged by state',
      .default = 'Qualitative ratings recoded to 1 (Good, Fair) and 0 (Poor) and averaged by state'
    ),
    axis_name = variable_name,
    dimension = "environment",
    index = 'water quality',
    indicator = 'quantity / quality of surface and shallow subsurface water',
    units = 'proportion',
    scope = 'national',
    resolution = 'state',
    year = '2019',
    latest_year = '2019',
    updates = "5 years",
    source = 'U.S. Environmental Protection Agency, National Aquatic Resource Surveys, 2022',
    url = 'https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys'
  ) %>% 
  add_citation() %>% 
  select(-LEGAL_VALUES)

metas$rivers
get_str(metas$rivers)

# Go back and fix data names
results$rivers <- results$rivers %>% 
  mutate(variable_name = paste0('rivers', snakecase::to_upper_camel_case(variable_name)))



# USDA Bees ---------------------------------------------------------------


# Just want colonies for April 1st, 2024
raw <- read_csv('1_raw/usda/hcny0824/hcny_p08_t022.csv', skip = 5)

# Start on row 10, end on row 57
# Only want columns 3 (state), 4 (colony count)
# Then filter to NE states
bees <- raw[4:(nrow(raw) - 10), 3:4] %>% 
  setNames(c('state', 'colonies')) %>% 
  filter(state %in% unique(fips_key$state_name))

bees$state %>% unique
# Only have four states represented here. Not that useful, not including for now



# FSA Disaster Declarations -----------------------------------------------
## Secretary ---------------------------------------------------------------


# Secretary of Ag disaster declarations - load all
paths <- list.files(
  '1_raw/usda/fsa/disaster_declarations/sec/',
  full.names = TRUE
)
ag_raw <- map(paths, read_excel)
  

# Select cols 1, 5, and last, filter by fips to NE, combine all years
# Note that we are not keeping info on what kind of events they are. May regret
ag <- map(ag_raw, ~ {
  .x %>% 
    select(1, 5, contains('YEAR')) %>% 
    setNames(c('fips', 'des', 'year')) %>% 
    mutate(
      across(everything(), as.character),
      year = ifelse(year == '2011, 2012', '2012', year)
    ) %>% 
    filter(fips %in% fips_key$fips)
}) %>% 
  bind_rows()
get_str(ag)


# Want variables by county, state, whole system
# county first - number of unique events in each year
ag_county <- ag %>% 
  group_by(fips, year) %>% 
  summarize(n_des = length(unique(des))) %>% 
  arrange(fips)
get_str(ag_county)

# Get grid of all possibilities of counties and years - make sure no missing
old_fips <- fips_key %>% 
  filter_fips('old') %>% 
  pull(fips)
all_years <- c(2012:2020, 2022:2023)
grid <- expand.grid(fips = old_fips, year = all_years) %>% 
  mutate(across(everything(), as.character))

# Join back to grid, turn NA into 0
ag_county <- left_join(grid, ag_county) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.x), '0', .x)))
get_str(ag_county)

# Arrange like a regular variable
ag_county <- ag_county %>% 
  rename(nFsaSecDisasters = n_des) %>% 
  pivot_longer(
    cols = nFsaSecDisasters,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(ag_county)

# [] Should we get state and system as well?

# Save it
results$fsa_sec <- ag_county



## Presidential ------------------------------------------------------------


# Presidential disaster declarations
paths <- list.files(
  '1_raw/usda/fsa/disaster_declarations/pres/',
  full.names = TRUE
)
pres_raw <- map(paths, read_excel)
  
# Select cols 1, 5, and last, filter by fips to NE, combine all years
# Note that we are not keeping info on what kind of events they are. May regret
pres <- map(pres_raw, ~ {
  .x %>% 
    select(1, 5, contains('YEAR')) %>% 
    setNames(c('fips', 'des', 'year')) %>% 
    mutate(
      across(everything(), as.character)
    ) %>% 
    filter(fips %in% fips_key$fips)
}) %>% 
  bind_rows()
get_str(pres)


# Want variables by county, state, whole system
# county first - number of unique events in each year
pres_county <- pres %>% 
  group_by(fips, year) %>% 
  summarize(n_des = length(unique(des))) %>% 
  arrange(fips)
get_str(pres_county)

# Get grid of all possibilities of counties and years - make sure no missing
old_fips <- fips_key %>% 
  filter_fips('old') %>% 
  pull(fips)
all_years <- c(2017:2020, 2023)
grid <- expand.grid(fips = old_fips, year = all_years) %>% 
  mutate(across(everything(), as.character))

# Join back to grid, turn NA into 0
pres_county <- left_join(grid, pres_county) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.x), '0', .x)))
get_str(pres_county)

# Arrange like a regular variable
pres_county <- pres_county %>% 
  rename(nFsaPresDisasters = n_des) %>% 
  pivot_longer(
    cols = nFsaPresDisasters,
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  arrange(year)
get_str(pres_county)

# Save
results$fsa_pres <- pres_county



## Metadata ----------------------------------------------------------------

names(results)
results$fsa_pres$variable_name %>% unique
results$fsa_sec$variable_name %>% unique

metas$fsa <- data.frame(
  variable_name = c(
    'nFsaPresDisasters',
    'nFsaSecDisasters'
  ),
  metric = c(
    'Number of FSA presidential disaster declarations',
    'Number of FSA Agriculture Secretary disaster declarations'
  ),
  definition = c(
    'Number of unique disaster declarations made by the Secretary of Agriculture. Used to trigger eligibility for emergency loans and FSA disaster assistance programs.',
    'Number of unique Presidential major disaster declarations. Used to trigger eligibility for emergency loans and FSA disaster assistance programs.'
  ),
  axis_name = c(
    'FSA Pres. Disasters',
    'FSA Sec. Disasters'
  ),
  dimension = "environment",
  index = 'water stability',
  indicator = 'days / events of extremes',
  units = 'count',
  scope = 'national',
  resolution = 'county',
  year = c(
    paste(c(2017:2020, 2023), collapse = ', '),
    paste(c(2012:2020, 2022:2023), collapse = ', ')
  ),
  latest_year = '2023',
  updates = "annual",
  source = 'U.S. Department of Agriculture, Farm Service Agency Disaster Assistance',
  url = 'https://www.fsa.usda.gov/resources/disaster-assistance-program/disaster-designation-information'
) %>% 
  add_citation('December 11, 2024')

metas$fsa



# ERS State Ag Data -------------------------------------------------------


# Imports and exports by state
# https://www.ers.usda.gov/data-products/state-agricultural-trade-data/



## Imports -----------------------------------------------------------------


imports_raw <- read_csv('1_raw/usda/ers_state_trade/top_5_ag_imports_by_state.csv') %>% 
  setNames(c(snakecase::to_snake_case(names(.))))
get_str(imports_raw)

# Filter down to New England
# Only keep world totals of top 5 imports, not each individual one
# Also only keeping fiscal year total, not quarterly
imports <- fips_key %>% 
  select(fips, state_code) %>% 
  inner_join(imports_raw, by = join_by(state_code == state)) %>% 
  filter(country == 'World', fiscal_quarter == '0') %>%
  select(
    fips, 
    year = fiscal_year, 
    variable_name = commodity_name, 
    value = dollar_value
  )
get_str(imports)
# Note that this is pretty interesting, but not sure what we can really do with
# the product-specific data. It would be hard to organize into 5 metrics because
# they are not the same across each state. For now I guess we're just taking the
# world total?

# Group by state and year to get total world imports for top 5 products
# Also pivot to fit format 
imports <- imports %>% 
  group_by(fips, year) %>% 
  summarize(importsTopFive = sum(value)) %>% 
  pivot_longer(
    cols = importsTopFive,
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  mutate(value = value / 1e6)
get_str(imports)

# Save it
results$imports <- imports



## Exports -----------------------------------------------------------------


exports_raw <- read_csv('1_raw/usda/ers_state_trade/state_exports.csv') %>% 
  setNames(c(snakecase::to_snake_case(names(.))))
get_str(exports_raw)

# Filter to New England and clean
exports <- fips_key %>% 
  select(fips, state_name) %>% 
  filter(str_length(fips) == 2) %>% 
  inner_join(exports_raw, by = join_by(state_name == state)) %>% 
  select(-state_name, -units) %>% 
  mutate(
    variable_name = paste0('exports', snakecase::to_upper_camel_case(commodity)) %>% 
      str_remove('Exports$'),
    .keep = 'unused'
  )
get_str(exports)  

# Save 
results$exports <- exports



## Metadata ----------------------------------------------------------------


# Check vars for exports. do imports separately
vars <- results$exports$variable_name %>% 
  unique %>% 
  sort
vars

# Plain text versions of vars to use in definition
plain_vars <- vars %>% 
  str_remove('exports') %>% 
  snakecase::to_sentence_case() %>% 
  str_to_lower() %>% 
  case_when(
    . == 'total' ~ 'total exports',
    .default = .
  )
plain_vars

metas$import_export <- data.frame(
  variable_name = c(vars, 'importsTopFive'),
  definition = c(
    paste(
      'Value (in millions) of',
      plain_vars,
      'exports'
    ),
    'Value (in millions) of imports for top five agricultural commodity groups from outside of the United States. Note that this is not total imports, nor does it include imports from other states.'
  ),
  axis_name = c(
    paste(
      c(
        'Beef',
        'Broiler Meat', 
        'Corn',
        'Cotton',
        'Dairy',
        'Feed',
        'Fresh Fruit',
        'Processed Fruit',
        'Grain',
        'Hide and Skin',
        'Other Livestock',
        'Other Oilseed',
        'Other Plant',
        'Other Poultry',
        'Pork',
        'Rice',
        'Soybean Mean',
        'Soybean',
        'Tobacco',
        'Total Agricultural',
        'Total Animal',
        'Total Plant',
        'Tree Nut',
        'Vegetable Oil',
        'Fresh Vegetable',
        'Processed Vegetable',
        'Wheat'
      ),
      'Exports (million usd)'
    ),
    'Top Five Imports (million usd)'
  ),
  dimension = "production",
  index = 'imports vs exports',
  indicator = c(
    rep('total quantity exported', length(plain_vars)),
    'total quantity imported'
  ),
  units = 'million usd',
  scope = 'national',
  resolution = 'state',
  year = c(
    rep(paste0(results$exports$year %>% unique %>% sort, collapse = ', '), length(plain_vars)),
    paste0(results$imports$year %>% unique %>% sort, collapse = ', ')
  ),
  latest_year = c(
    rep(results$exports$year %>% max(), length(plain_vars)),
    results$imports$year %>% max
  ),
  updates = "annual",
  source = c(
    rep('US Department of Agriculture, Economic Research Service, State Agricultural Trade Data, State Exports', length(plain_vars)),
    'US Department of Agriculture, Economic Research Service, State Agricultural Trade Data, State Trade by Country of Origin and Destination'
  ),
  url = 'https://www.ers.usda.gov/data-products/state-agricultural-trade-data/',
  citation = paste0(
    'USDA ERS (2023). State Agricultural Trade Data. Accessed from ',
    'https://www.ers.usda.gov/data-products/state-agricultural-trade-data/, ',
    'December 16th, 2024.'
  )
) %>% 
  mutate(
    metric = variable_name %>% 
      str_remove('^exports|^imports') %>% 
      snakecase::to_sentence_case() %>% 
      str_to_lower(),
    metric = case_when(
      metric == 'top five' ~ 'value of top five imports',
      .default = paste(
        'Value of', metric, 'exports'
      )
    )
  )

metas$import_export



# Aggregate and Save ------------------------------------------------------


# Put metrics and metadata together into two single DFs
aggregate_metrics(results, metas)

# Check record counts
check_n_records(result, meta, 'other')

saveRDS(result, '5_objects/metrics/other.RDS')
saveRDS(meta, '5_objects/metadata/other_meta.RDS')

clear_data()
