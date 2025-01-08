#' Food Systems Data Warehouse
#' 2024-09-19
#' 
#' Pulling cleaned and processed data from FS Data Warehouse. Same data that is
#' used in the FAME food and ag mapper and explorer
#' https://allison-bauman.quarto.pub/usda-ams-datametrics/



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  janitor,
  tidyr,
  readr,
  stringr
)

# Initiate list of results and metadata
results <- list()
metas <- list()

fips_key <- readRDS('5_objects/fips_key.rds')
state_key <- readRDS('5_objects/state_key.rds')
all_fips <- readRDS('5_objects/all_fips.rds')

# Load functions
source('3_functions/wrangling/warehouse_utilities.R')
source('3_functions/pipeline_utilities.R')
source('3_functions/metadata_utilities.R')



# Local Sales -------------------------------------------------------------


# USDA Local Food Directories
# https://www.usdalocalfoodportal.com/#directories
local_sales <- read_csv('1_raw/food_systems_data_warehouse/df_localfoodsales.csv')
get_str(local_sales)


## Select relevant variables
local_sales$variable_name %>% unique %>% sort
vars <- local_sales$variable_name %>% 
  unique %>% 
  str_subset('sales_pct$|^number|valueadded') %>% 
  str_subset('inter', negate = TRUE) %>% 
  sort()
vars

# Pull them all
results$local_sales <- local_sales %>% 
  filter(variable_name %in% vars) %>% 
  rename_local_sales_vars() %>% 
  filter(fips %in% all_fips)
get_str(results$local_sales)


## Wrangle metadata from data warehouse
local_meta <- read.csv('1_raw/food_systems_data_warehouse/meta_localfoodsales.csv')
get_str(local_meta)

metas$local_sales <- local_meta %>% 
  wrangle_meta(vars) %>% 
  rename_local_sales_vars() %>% 
  mutate(
    dimension = c(
      rep('economics', 6),
      'production',
      'production',
      'production',
      'production'
    ),
    index = c(
      rep('distribution chain localness', 6),
      'production margins',
      'production margins',
      'production margins',
      'production margins'
    ),
    indicator = c(
      'local sales',
      'direct to consumer sales',
      'local sales',
      'direct to consumer sales',
      'direct to consumer sales',
      'direct to consumer sales',
      rep('value added market', 4)
    ),
    axis_name = c(
      'Agritourism sales (%)',
      'D2C sales (% of total)',
      'Local sales (% of total)',
      'Number of CSAs',
      'Number of farmers markets',
      'Number of on-farm markets',
      'Number of farms with value-added sales',
      'Farms with value-added sales (%)',
      'Number of farms with value-added sales',
      'Value-added sales (% of total)'
    ),
    units = case_when(
      str_detect(variable_name, '^n') ~ 'count',
      str_detect(variable_name, 'Perc$') ~ 'percentage',
      variable_name == 'valueAddedSales' ~ 'usd'
    ),
    scope = 'national',
    resolution = 'county, state',
    warehouse = TRUE,
    year = get_all_years(results$local_sales),
    latest_year = get_max_year(results$local_sales)
  )

get_str(metas$local_sales)



# Access ------------------------------------------------------------------


access <- read_csv(
  '1_raw/food_systems_data_warehouse/df_foodaccess.csv',
  show_col_types = FALSE
) %>% 
  filter(fips %in% all_fips)
get_str(access)

## Explore variables
access$variable_name %>% unique %>% sort

# Vector of relevant variables
access_variables <- c(
  'child_food_insecurity_rate',
  'groc',
  'grocpth',
  'overall_food_insecurity_rate',
  'snap_all_eligible_people_percent',
  'wic_coverage_rate',
  'wic_eligibility_rate',
  'specspth',
  'snapspth',
  'wicspth'
) %>% 
  sort()

# Filter to only relevant variables, then rename
results$access <- access %>% 
  filter(variable_name %in% access_variables) %>% 
  rename_access_vars()
get_str(results$access)
results$access$variable_name %>% unique

## Edit Metadata
metas$access <- read_csv(
  "1_raw/food_systems_data_warehouse/meta_foodaccess.csv"
) %>% 
  wrangle_meta(access_variables) %>% 
  rename_access_vars() %>% 
  mutate(
    dimension = 'health',
    index = 'food security',
    indicator = case_when(
      str_detect(variable_name, '^groc') ~ 'food access',
      .default = 'food affordability'
    ),
    units = case_when(
      str_detect(variable_name, 'Insec') ~ 'proportion',
      str_detect(variable_name, 'Perc') ~ 'percentage',
      str_detect(variable_name, 'PTH$') ~ 'density',
      str_detect(variable_name, '^n') ~ 'count',
      .default = NA
    ),
    scope = 'national',
    warehouse = TRUE,
    resolution = get_resolution(results$access),
    year = get_all_years(results$access),
    latest_year = get_max_year(results$access)
  )

get_str(metas$access)



# Business Development ----------------------------------------------------


infra <- read_csv('1_raw/food_systems_data_warehouse/df_business_dev_infra.csv') %>% 
  mutate(fips = as.character(fips)) %>% 
  filter(fips %in% all_fips)
get_str(infra)

infra$variable_name %>% 
  unique %>% 
  sort
  
# Get relevant vars
vars <- c(
  'anaerobic_digestion_facilities',
  'number_food_hubs',
  'number_composting_facilities',
  'number_meat_processing',
  'private_semi_private_refrigerated_warehouses',
  'public_refrigerated_warehouses'
)

# Select relevant vars
results$dist_chain_capacity <- infra %>% 
  filter(variable_name %in% vars) %>% 
  rename_infra_vars()
get_str(results$dist_chain_capacity)


## Metadata
metas$business_infrastructure <- read_csv(
  '1_raw/food_systems_data_warehouse/meta_business_dev_infra.csv'
) %>% 
  wrangle_meta(vars) %>% 
  rename_infra_vars() %>% 
  mutate(
    dimension = 'economics',
    index = 'distribution chain localness',
    indicator = 'distribution chain capacity',
    axis_name = c(
      'Number of anaerobic digesters',
      'Number of compost facilities',
      'Number of food hubs',
      'Number of meat processors',
      'Number of private refrig.',
      'Number of public refrig.'
    ),
    units = 'count',
    scope = 'national',
    resolution = get_resolution(results$dist_chain_capacity),
    warehouse = TRUE,
    year = get_all_years(results$dist_chain_capacity),
    latest_year = get_max_year(results$dist_chain_capacity)
  )

get_str(metas$business_infrastructure)
names(metas$business_infrastructure)



# Labor -------------------------------------------------------------------


labor <- read_csv('1_raw/food_systems_data_warehouse/df_labor.csv') %>% 
  filter(fips %in% all_fips)
get_str(labor)

# Explore variables
(all_vars <- labor$variable_name %>% unique %>% sort)

# Identify relevant vars
vars <- labor$variable_name %>% 
  str_subset('^median|^women|naics') %>% 
  unique
vars

# Economics > community economy > wage rate
results$labor <- labor %>% 
  filter(variable_name %in% vars) %>% 
  rename_labor_vars()

get_str(results$labor)
results$labor$variable_name %>% unique


## Metadata
metas$labor <- read_csv(
  '1_raw/food_systems_data_warehouse/meta_labor.csv'
) %>% 
  wrangle_meta(vars) %>% 
  rename_labor_vars() %>% 
  mutate(
    dimension = 'economics',
    index = 'community economy',
    indicator = 'wage rate',
    units = ifelse(str_detect(variable_name, '^median'), 'usd', 'percentage'),
    units = case_when(
      str_detect(variable_name, '^n') ~ 'count',
      str_detect(variable_name, 'Wage|Earn') ~ 'usd',
      str_detect(variable_name, 'Perc') ~ 'percentage',
      str_detect(variable_name, '^lq') ~ 'ratio',
      str_detect(variable_name, 'EmpLvl') ~ 'count'
    ),
    scope = 'national',
    resolution = 'county',
    warehouse = TRUE,
    updates,
    axis_name = variable_name,
    year = get_all_years(results$labor),
    latest_year = get_max_year(results$labor),
    resolution = get_resolution(results$labor)
  )

get_str(metas$labor)



# Consolidate ------------------------------------------------------------


get_str(results)
map(results, get_str)

# Fix year to all be character
results <- map(results, ~ mutate(.x, year = as.character(year)))

# Aggregate them and lose extraneous columns
result <- results %>% 
  bind_rows() %>% 
  unique() %>% 
  select(-c(
    category, 
    topic_area,
    county_name,
    state_name
  ))
get_str(result)


## Combine metadata
map(metas, get_str)

# All the warehouse variables should be TRUE from this script
meta <- metas %>% 
  bind_rows() %>% 
  mutate(warehouse = TRUE)
get_str(meta)

check_n_records(result, meta, 'Warehouse')

saveRDS(result, '5_objects/metrics/data_warehouse.RDS')
saveRDS(meta, '5_objects/metadata/data_warehouse.RDS')

clear_data()
gc()
