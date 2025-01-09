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

source('3_functions/metadata_utilities.R')
source('3_functions/pipeline_utilities.R')
fips_key <- readRDS('5_objects/fips_key.rds')

results <- list()
metas <- list()



# EPA GHGs ----------------------------------------------------------------


# https://www.epa.gov/ghgemissions/state-ghg-emissions-and-removals
# Go to 'download consolidated data for all states (zip)'

# First UN sectors
un_raw <- read_xlsx(
  '1_raw/epa/state_ghgs_1990_2022/AllStateGHGData90-22_v082924.xlsx',
  sheet = 2
) %>% 
  unique()
get_str(un_raw)

# Narrow down to New England, Agriculture, relevant columns
un <- un_raw %>% 
  select(
    sector:sub_category_3, 
    state = geo_ref,
    ghg_category,
    starts_with('Y')
  ) %>% 
  filter(sector == 'Agriculture') %>% 
  inner_join(
    select(fips_key, fips, state_code), 
    by = join_by(state == state_code)
  ) %>% 
  select(-state, -sector) # Don't need sector anymore - all agriculture
get_str(un)

# Combine categories so we can use them as definition later. It will also become
# variable name I guess. Leaving it as definition for now.
un <- un %>% 
  mutate(
    # Turn NA into '' so we can paste them together
    across(starts_with('sub_'), ~ ifelse(is.na(.x), '', .x)),
    # Paste unique identifiers as deep as we need to go
    definition = paste(
      subsector, 
      category, 
      sub_category_1, 
      sub_category_2, 
      sub_category_3, 
      sep = ' > '
    ),
    definition = case_when(
      str_detect(definition, '^CO2') ~ paste0(
        str_sub(definition, end = 4),
        str_to_lower(str_sub(definition, start = 5))
      ),
      str_detect(definition, '^CO2', negate = TRUE) ~ paste(
        ghg_category, 
        'emissions from',
        str_to_lower(definition)
      ),
      .default = NA
    ),
    definition = str_remove_all(definition, ' > $| >  > ') # remove blanks
  )
get_str(un)
dim(un)

# Calculate aggregates by category in separate df
subsector_totals <- un %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "year",
    names_prefix = "Y",
    values_to = "value"
  ) %>%
  # aggregating to subsector
  group_by(fips, year, subsector, ghg_category) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(definition = paste0(
    'Subsector total of ', 
    ghg_category, 
    ' emissions from agriculture: ', 
    str_to_lower(subsector)
  )) %>%
  select(-subsector)
get_str(subsector_totals)
dim(subsector_totals)

# Another set where we get CO2, CH4, and N2O totals per fips and year
ghg_totals <- subsector_totals %>% 
  group_by(fips, year, ghg_category) %>% 
  summarize(value = sum(value, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(definition = paste0('Total ', ghg_category, ' emissions from agriculture')) %>% 
  select(-ghg_category)
get_str(ghg_totals)

# Put main data in long format
un <- un %>%
  select(-c(subsector:ghg_category)) %>% 
  pivot_longer(
    cols = starts_with('Y'),
    names_to = 'year',
    values_to = 'value'
  ) %>%
  mutate(year = str_remove(year, 'Y'))
get_str(un)

# Now we can combine them all into one long format df
# Also add variable names here. Save it to use in metadata
# Also adding teragrams to end of each definition
un_definitions <- un %>% 
  bind_rows(select(subsector_totals, -ghg_category)) %>% 
  bind_rows(ghg_totals) %>% 
  mutate(
    definition = paste(definition, '(Tg)'),
    
    metric = str_remove(
      definition, 
      'manure management > |field burning of agricultural residues > |enteric fermentation > '
    ) %>%
      str_remove('total of ') %>% 
      str_remove('agricultural ') %>% 
      str_remove('carbon-containing ') %>% 
      str_replace_all(' > ', ' '),
    
    variable_name = metric %>% 
      str_remove('Total ') %>% 
      str_remove('emissions ') %>% 
      str_replace('from agriculture ', 'from ag ') %>% 
      str_remove('from manure management ') %>% 
      str_remove('from agriculture: ') %>% 
      str_replace('from field burning of residues ', 'burning residues ') %>% 
      str_remove('from liming, urea application and other ') %>% 
      str_remove('from enteric ') %>% 
      str_remove('residues ') %>% 
      str_remove('from direct and indirect n2o emissions from soils agricultural ') %>% 
      str_replace('other other ', 'other ') %>% 
      str_remove('and roots other ') %>% 
      str_replace('management ', 'manage ') %>% 
      str_replace('cropland ', 'crop ') %>% 
      str_replace('grassland ', 'grass ') %>% 
      str_remove(' \\(Tg\\)'),
    
    variable_name = paste0(
      str_sub(variable_name, end = 3),
      snakecase::to_upper_camel_case(str_sub(variable_name, start = 4))
    )
  )

un_definitions$variable_name %>% unique
get_str(un_definitions)
un_definitions$definition %>% unique
un_definitions$metric %>% unique


# Pull out just the metric values for results list
un_metrics <- un_definitions %>% 
  select(fips, variable_name, value, year)
get_str(un_metrics)

# Save it
results$ghgs <- un_metrics


## Metadata ----------------------------------------------------------------


# Check vars
(vars <- get_vars(un_metrics))

# Reformat the definitions and metrics into wide for metadata
get_str(un_definitions)
metas$ghgs <- un_definitions %>% 
  select(variable_name, metric, definition, year) %>% 
  group_by(variable_name, metric, definition) %>% 
  summarize(
    latest_year = max(unique(year)),
    year = paste0(year, collapse = ', ')
  ) %>% 
  mutate(
    axis_name = variable_name,
    dimension = 'environment',
    index = 'air quality',
    indicator = 'carbon, ghg, nutrients',
    units = 'Tg',
    scope = 'national',
    resolution = 'state',
    updates = "annual",
    source = 'U.S. Environmental Protection Agency State GHG Data (2024).',
    url = 'https://www.epa.gov/ghgemissions/state-ghg-emissions-and-removals',
    warehouse = FALSE
  ) %>% 
  add_citation(access_date = '2024-12-18')

metas$ghgs



# EPA NARS ----------------------------------------------------------------
## Lakes -------------------------------------------------------------------


## Load
lakes <- read_csv('1_raw/epa/nars/lakes_data_for_population_estimates_2022.csv')

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
  add_citation(access_date = '2024-11-14') %>% 
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
  add_citation(access_date = '2024-11-14') %>% 
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

# Add a sum of all exports for a total

# Save 
results$exports <- exports



## Metadata ----------------------------------------------------------------


# Check vars for exports. do imports separately
(vars <- get_vars(results$exports))

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
    get_all_years(results$exports),
    get_all_years(results$imports)
  ),
  latest_year = c(
    get_max_year(results$exports),
    get_max_year(results$imports)
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
  
  # Build metrics out of variable_names 
  # Second step because we need to finish data frame first
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



# USDA F2S Census ---------------------------------------------------------


# USDA Farm to School Census
# https://farmtoschoolcensus.fns.usda.gov/census-results/census-data-explorer

# Load national summary data. Need specific pages for the data we want
# Have to do these manually to get right cells. Unfortunate
path <- '1_raw/usda/f2s_census/ops-f2s-2023NationalStateDataWorkbook-120924.xlsx'
census <- list()
census$sfa_participation <- read_xlsx(
  path, 
  sheet = '2. Number of SFAs',
  range = 'A4:F61'
) %>% 
  select(state = 1, sfaFarmToSchool = 4)
census$culture <- read_xlsx(
  path, 
  sheet = '6. F2S Activities',
  range = 'A5:T62'
) %>% 
  select(state = 1, sfaCulturallyRelevant = 20)
census$activities <- read_xlsx(
  path, 
  sheet = '7. F2S Activity Categories',
  range = 'A4:D61'
) %>% 
  select(state = 1, sfaServeLocal = 4)
census$local_spending <- read_xlsx(
  path, 
  sheet = '20. Local Foods Spending',
  range = 'A4:D61'
) %>% 
  select(state = 1, sfaLocalFoodCosts = 4)
get_str(census)

# Now we can put them together
all_states <- fips_key$state_code[!is.na(fips_key$state_code)]
census <- map(census, ~ {
  .x %>% 
    mutate(state = ifelse(state == 'National', 'US', state)) %>% 
    filter(state %in% all_states) %>% 
    left_join(select(fips_key, fips, state_code), by = join_by(state == state_code)) %>% 
    select(-state)
}) %>% 
  reduce(inner_join) %>% 
  mutate(sfaLocalFoodCosts = as.numeric(sfaLocalFoodCosts)) %>% 
  mutate(across(is.numeric, ~ format(round(.x, 2), nsmall = 2)))
get_str(census)

# Pivot longer, add year, format like all other variables
census <- census %>%
  pivot_longer(
    cols = !fips,
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  mutate(year = '2023')
get_str(census)  

# Save to results
results$census <- census



## Metadata ----------------------------------------------------------------


(vars <- get_vars(census))

metas$census <- data.frame(
  variable_name = get_vars(census),
  metric = c(
    'SFAs serving culturally relevant food',
    'SFAs with Farm to School program',
    'SFAs percentage of spending on local food',
    'SFAs serving local food'
  ),
  definition = c(
    'Percentage of all School Food Authorities growing or serving culturally relevant foods.',
    'Percentage of all School Food Authorities participating in a Farm to School program.',
    'Percentage of total food costs spent on local foods among School Food Authorities which participate in a Farm to School program.',
    'Percentage of all School Food Authorities serving local foods. Note that the definition of "local" can very by SFA.'
  ),
  axis_name = c(
    'SFA Culturally Relevant Food (%)',
    'SFA with Farm to School (%)',
    'SFA Local Food Costs (%)',
    'SFA Serving Local Food (%)'
  ),
  dimension = 'health',
  index = 'food security',
  indicator = c(
    'access to culturally appropriate food',
    'dietary quality',
    'dietary quality',
    'dietary quality'
  ),
  units = 'percentage',
  scope = 'national',
  resolution = 'state',
  year = get_all_years(census),
  latest_year = get_max_year(census),
  updates = "~ 4 years",
  source = 'U.S. Department of Agriculture Food and Nutrition Service Farm to School Program Census (2023).',
  url = 'https://farmtoschoolcensus.fns.usda.gov/census-results/census-data-explorer'
) %>% 
  add_citation(access_date = '2024-12-18')
  
metas$census



# Aggregate and Save ------------------------------------------------------


# Put metrics and metadata together into two single DFs
out <- aggregate_metrics(results, metas)

# Check record counts
check_n_records(out$result, out$meta, 'other')

saveRDS(out$result, '5_objects/metrics/other.RDS')
saveRDS(out$meta, '5_objects/metadata/other_meta.RDS')

clear_data()
gc()