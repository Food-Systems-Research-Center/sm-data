# EPA GHG data
# 2024-09-10



# Houskeeping -------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr,
  tidyr,
  snakecase
)

source('3_functions/add_citation.R')
source('3_functions/check_n_records.R')
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
      metric = paste0(var_name, '(Economic Sectors)'),
      variable_name = paste0(
        'mmtCo2',
        var_name %>%
          str_split_i(',', 1) %>%
          to_lower_camel_case(),
        'Inv'
      )
    )
})
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
      metric = paste0(var_name, '(Economic Sectors)'),
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
      'Divided by economic sector or inventory sectory, which is consistent with international standards.'
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



# Aggregate and Save ------------------------------------------------------


result <- results %>% 
  map(\(x) mutate(x, value = as.character(value))) %>% 
  bind_rows()
get_str(result)

meta <- metas %>% 
  bind_rows()
get_str(meta)

# Check record counts
check_n_records(result, meta, 'GHG')

saveRDS(result, '5_objects/metrics/other.RDS')
saveRDS(meta, '5_objects/metadata/other_meta.RDS')

clear_data()
