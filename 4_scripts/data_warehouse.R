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

# NE state names
ne_state_names <- c(
  'Vermont',
  'New Hampshire',
  'Maine',
  'Massachusetts',
  'Rhode Island',
  'Connecticut'
)

# fips for before and after CT changes
fips_2021 <- readRDS('5_objects/fips_2021.rds')
fips_2024 <- readRDS('5_objects/fips_2024.rds')
fips_key <- readRDS('5_objects/fips_key.rds')

# Helper function
source('3_functions/wrangle_warehouse_meta.R')



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
  filter(variable_name %in% vars)
get_str(results$local_sales)


## Wrangle metadata from data warehouse
local_meta <- read_csv(
  '1_raw/food_systems_data_warehouse/meta_localfoodsales.csv',
  show_col_types = FALSE
)

# Rename variables [] do we want to do this or not?
# results$local_sales <- results$local_sales %>% 
#   mutate(variable_name = case_when(
#     str_detect(variable_name, '^agri') ~ 'agTourSalesPct',
#     str_detect(variable_name, '^d2c') ~ 'd2cSalesPct',
#     str_detect(variable_name, '^local') ~ 'localSalesPct',
#     str_detect(variable_name, '^number_csa') ~ 'nCSA',
#     str_detect(variable_name, '^number_far') ~ 'nFarmersMarket',
#     str_detect(variable_name, '^number_on') ~ 'nOnFarmMarket',
#     variable_name == 'valueadded_farms' ~ 'nValueAddedFarms',
#     variable_name == 'valueadded_farms_pct' ~ 'valueAddedFarmsPct',
#     variable_name == 'valueadded_sales' ~ 'valueAddedSales',
#     variable_name == 'valueadded_sales_pct' ~ 'valueAddedSalesPct'
#   ))

metas$local_sales <- local_meta %>% 
  wrangle_meta(vars) %>%  
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
    units = case_when(
      str_detect(variable_name, '_pct$') ~ 'percentage',
      str_detect(variable_name, '^number|_farms') ~ 'count',
      str_detect(variable_name, 'sales$') ~ 'usd'
    ),
    scope = 'national',
    resolution = 'county',
    quality = 2,
    warehouse = TRUE
  )

get_str(metas$local_sales)
rm(local_sales)



# Access ------------------------------------------------------------------


access <- read_csv(
  '1_raw/food_systems_data_warehouse/df_foodaccess.csv',
  show_col_types = FALSE
) %>% 
  filter(state_name %in% c(ne_state_names, 'US')) %>% 
  unique()
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

# Filter to only relevant variables
results$access <- access %>% 
  filter(variable_name %in% access_variables)


## Edit Metadata
metas$access <- read_csv(
  "1_raw/food_systems_data_warehouse/meta_foodaccess.csv"
) %>% 
  wrangle_meta(access_variables) %>% 
  unique() %>% 
  mutate(
    dimension = 'health',
    index = 'food security',
    indicator = case_when(
      str_detect(variable_name, '^groc') ~ 'food access',
      .default = 'food affordability'
    ),
    units = case_when(
      str_detect(variable_name, 'rate$') ~ 'proportion',
      str_detect(variable_name, 'percent$') ~ 'percentage',
      str_detect(variable_name, 'pth$') ~ 'density',
      str_detect(variable_name, 'groc$') ~ 'count',
      .default = NA
    ),
    scope = 'national',
    resolution = 'county',
    warehouse = TRUE,
    quality = 2
  )

get_str(metas$access)



# Business Development ----------------------------------------------------


infra <- read_csv(
  '1_raw/food_systems_data_warehouse/df_business_dev_infra.csv',
  show_col_types = FALSE
) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(
    (fips %in% fips_all) | (state_name %in% ne_state_names)
  )

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
  filter(variable_name %in% vars)
get_str(results$dist_chain_capacity)


## Metadata
metas$business_infrastructure <- read_csv(
  '1_raw/food_systems_data_warehouse/meta_business_dev_infra.csv'
) %>% 
  wrangle_meta(vars) %>% 
  mutate(
    dimension = 'economics',
    index = 'distribution chain localness',
    indicator = 'distribution chain capacity',
    units = 'count',
    scope = 'national',
    resolution = c(rep('county', 4), rep('state', 2)),
    quality = c(2, 2, 1, 2, 2, 2),
    warehouse = TRUE
  )

get_str(metas$business_infrastructure)
metas$business_infrastructure

rm(infra)



# Labor -------------------------------------------------------------------


labor <- read_csv(
    '1_raw/food_systems_data_warehouse/df_labor.csv',
    show_col_types = FALSE
  ) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter((fips %in% fips_all) | (state_name %in% ne_state_names))
get_str(labor)

# Explore variables
labor$variable_name %>% unique %>% sort

# Identify relevant vars
vars <- labor$variable_name %>% 
  str_subset('^median|^women') %>% 
  unique

# Economics > community economy > wage rate
results$labor <- labor %>% 
  filter(variable_name %in% vars)
get_str(results$labor)
results$labor$year %>% unique

## Metadata
metas$labor <- read_csv(
  '1_raw/food_systems_data_warehouse/meta_labor.csv'
) %>% 
  wrangle_meta(vars) %>% 
  mutate(
    dimension = 'economics',
    index = 'community economy',
    indicator = 'wage rate',
    units = ifelse(str_detect(variable_name, '^median'), 'dollars', 'percentage'),
    scope = 'national',
    resolution = 'county',
    quality = 2,
    warehouse = TRUE
  )

metas$labor
rm(labor)


# Community Resources -----------------------------------------------------


# cap <- read_csv(
#   '1_raw/food_systems_data_warehouse/df_community_resources.csv',
#   show_col_types = FALSE
# )
# get_str(cap)
# 
# # explore variables
# cap %>% 
#   select(category, topic_area, variable_name, fips) %>% 
#   unique %>% 
#   filter(str_detect(variable_name, 'nccs')) %>% 
#   print(n = 100)


# Consolidate ------------------------------------------------------------


get_str(results)
map(results, get_str)

result <- results %>% 
  bind_rows() %>% 
  unique() %>% 
  select(-c(
    category, 
    topic_area,
    county_name,
    state_name
  )) %>% 
  filter(fips %in% fips_key$fips)
get_str(result)

map(metas, get_str)
meta <- metas %>% 
  bind_rows %>% 
  rename(definition = variable_definition)
get_str(meta)

saveRDS(result, '5_objects/metrics/data_warehouse.RDS')
saveRDS(meta, '5_objects/metadata/data_warehouse.RDS')
