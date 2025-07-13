# NASS
# 2025-07-10 update


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  tidyr,
  vegan
)



# Description -------------------------------------------------------------


# Taking our API calls from nass_api.R and combining them. Using the nass 
# api parameters to start metadata
census <- readRDS('5_objects/api_outs/neast_nass_census_2002_2022.rds')
survey <- readRDS('5_objects/api_outs/neast_nass_survey_2002_2022.rds')

# Load nass api parameters to build metadata
nass_params <- read_csv('5_objects/api_parameters/nass_api_parameters.csv')



# Wrangle -----------------------------------------------------------------


bound <- bind_rows(census, survey)
get_str(bound)

# Pull relevant variables, create a single FIPS variable where counties have 5
# digits and states have 2
dat <- bound %>% 
  select(
    agg_level_desc,
    short_desc,
    value = Value,
    year, 
    county_ansi,
    state_ansi,
    cv = `CV (%)`,
    unit_desc
  ) %>% 
  mutate(
    fips = case_when(
      agg_level_desc == 'COUNTY' ~ paste0(state_ansi, county_ansi),
      agg_level_desc == 'STATE' ~ state_ansi,
      .default = NA
    ),
    .keep = 'unused'
  )
get_str(dat)

# Join with api parameter doc to get variable names
dat <- nass_params %>% 
  select(short_desc, variable_name) %>% 
  right_join(dat)
get_str(dat)

# For those without variable names, create them from short desc
dat <- dat %>% 
  mutate(
    variable_name = case_when(
      is.na(variable_name) ~ snakecase::to_lower_camel_case(short_desc),
      .default = variable_name
    )
  )
get_str(dat)

# Reduce unnecessary variables
dat <- dat %>% 
  select(
    variable_name,
    fips,
    year,
    value
  )
get_str(dat)



# Calculate New Variables -------------------------------------------------


# Doing this one at a time to avoid hangups pivoting wider with uneven sized
# data
(vars <- meta_vars(dat))

# Function to add a derived variable
calculate_var <- function(df,
                          num,
                          denom,
                          name) {
  out <- df %>% 
    filter(variable_name %in% c(num, denom)) %>% 
    pivot_wider(
      id_cols = c(fips, year),
      names_from = variable_name,
      values_from = value
    ) %>% 
    rowwise() %>% 
    mutate(
      !!sym(name) := !!sym(num) / !!sym(denom),
      .keep = 'unused'
    )
  cat('\nRange:', range(out[[name]], na.rm = TRUE))
  
  out <- out %>% 
    pivot_longer(
      cols = !c(fips, year),
      values_to = 'value',
      names_to = 'variable_name'
    )
  bind_rows(df, out)
}

get_str(dat)

# Add value added sales as a proportion of total sales
dat <- calculate_var(
  dat,
  'totalSalesValueAddedDirect',
  'totalSalesCommodities',
  'd2cSalesPropTotal'
)

# Value added retail as a proportion of all sales
dat <- calculate_var(
  dat,
  'totalSalesValueAddedDirect',
  'totalSalesCommodities',
  'salesValueAddedDirectPropTotal'
)

# Value added wholesale as a proportion of all sales
dat <- calculate_var(
  dat,
  'totalSalesValueAddedWholesale',
  'totalSalesCommodities',
  'salesValueAddedWholesalePropTotal'
)

# Income from agritourism as proportion of all income
dat <- calculate_var(
  dat,
  'totalIncomeAgTourismRecreation',
  'totalFarmIncome',
  'incomeAgTourismRecPropTotal'
)

# Fertilizer expenses as proportion of total expenses
dat <- calculate_var(
  dat,
  'expensesFertilizerLimeSoilCond',
  'totalOperatingExpenses',
  'fertExpensePropTotalExpense'
)

# Fuels as proportion of total expenses
dat <- calculate_var(
  dat,
  'nOpsFuelExpenses',
  'totalFuelExpenses',
  'fuelExpensePropTotalExpense'
)
# This might be hinky

# Female to male producer ratio
dat <- calculate_var(
  dat,
  'nFemProducers',
  'nMaleProducers',
  'ftmProdRatio'
)

# Income from crop and animal insurance as a proportion of total
dat <- calculate_var(
  dat,
  'totalIncomeCropAnimalInsurance',
  'totalFarmIncome',
  'incomeCropAnimalInsurancePropTotal'
)


## Producer racial diversity with Shannon index of producer races
get_str(dat)

# Prepare 
out <- dat %>% 
  filter(str_detect(variable_name, '^n.+Producers$')) %>% 
  pivot_wider(
    id_cols = c(fips, year),
    values_from = value,
    names_from = variable_name
  ) %>% 
  mutate(across(!c(fips, year), ~ ifelse(is.na(.x), 0, .x)))
get_str(out)

out$producerRacialDiversity <- out %>% 
  select(where(is.numeric)) %>% 
  diversity()
get_str(out) 

# Make it long again to add back into dat
dat <- out %>% 
  select(fips, year, producerRacialDiversity) %>% 
  pivot_longer(
    cols = producerRacialDiversity,
    names_to = 'variable_name',
    values_to = 'value'
  ) %>% 
  bind_rows(dat)
get_str(dat)  


## Total sales (animal sales + crop sales)
dat <- dat %>% 
  filter(variable_name %in% c('salesAnimal', 'salesCrop')) %>% 
  pivot_wider(
    id_cols = c(fips, year),
    values_from = value,
    names_from = variable_name
  ) %>% 
  mutate(
    salesAnimalAndCrop = salesAnimal + salesCrop,
    .keep = 'unused'
  ) %>% 
  pivot_longer(
    cols = !c(fips, year),
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  bind_rows(dat)
get_str(dat)



# Metadata ----------------------------------------------------------------


get_str(dat)
get_str(nass_params)
(vars <- meta_vars(dat))
dat_df <- data.frame(variable_name = vars)

# Join nass params with variable names from dat to start metadata
meta <- nass_params %>% 
  filter(source_desc == 'CENSUS') %>%
  right_join(dat_df) %>% 
  select(-ends_with('desc'), -note)
get_str(meta)

# Work off of this to fill in rest of metadata
meta <- meta %>% 
  mutate(
    dimension = case_when(
      is.na(dimension) & str_detect(variable_name, 'Yield') ~ 'production',
      .default = dimension
    ),
    index = case_when(
      is.na(index) & str_detect(variable_name, 'Yield') ~ 'production margins',
      .default = dimension
    ),
    indicator = case_when(
      is.na(indicator) & str_detect(variable_name, 'Yield') ~ 'yield',
      .default = indicator
    ),
    axis_name = case_when(
      is.na(axis_name) ~ variable_name,
      .default = axis_name
    )
  ) %>% 
  
  # Fresh calculated variables
  mutate(
    latest_year = meta_latest_year(dat),
    year = meta_years(dat),
    resolution = meta_resolution(dat),
    scope = 'national',
    updates = '5 years',
    source = paste0(
      "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
      "(2024). 2022 Census of Agriculture."
    ),
    url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/',
  ) %>% 
  meta_citation(date = '2025-07-10')
get_str(meta)



# Aggregate ---------------------------------------------------------------


# Check record counts
check_n_records(dat, meta, 'nass')

saveRDS(dat, '5_objects/metrics/nass.RDS')
saveRDS(meta, '5_objects/metadata/nass_meta.RDS')

clear_data(gc = TRUE)

