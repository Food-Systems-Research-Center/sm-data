# Census API
# 2024-10-02

# Variables list: https://api.census.gov/data/2023/acs/acs1/variables.html


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  readr,
  tibble,
  janitor,
  tidyr,
  stringr
)

# lists of results
results <- list()
metas <- list()


# Rework ------------------------------------------------------------------
## Wrangle -----------------------------------------------------------------


out <- readRDS('5_objects/api_outs/acs5_neast_counties_states_2008_2023.rds')
get_str(out)

# Make normal fips column
dat <- out %>% 
  mutate(fips = paste0(state, ifelse(is.na(county), '', county))) %>% 
  select(-c(state, county))
get_str(dat)

# Check ranges for weird values
map(dat, range, na.rm = TRUE)
# median income is hinky, also housing year

# Recode -666666666 to missing for income and 0 to missing for housing year
dat <- dat %>% 
  mutate(
    across(everything(), ~ case_when(
      .x == -666666666 | .x == 0 ~ NA,
      .default = .x
    ))
  )
get_str(dat)           
map(dat, range, na.rm = TRUE)

# Calculations to get proportions for education, housing, earnings
dat <- dat %>% 
  mutate(
    edPercHSGED = ((edTotalHS + edTotalGED) / edTotal) * 100,
    edPercBS = (edTotalBS / edTotal) * 100,
    vacancyRate = (nHousingVacant / nHousingUnits) * 100,
    womenEarnPercMenFFF = (medianFemaleEarningsFFF / medianMaleEarningsFPS) * 100,
    womenEarnPercMenFPS = (medianFemaleEarningsFPS / medianMaleEarningsFPS) * 100,
    disconnectedYouth = (nMaleNotEnrolledHSGradNotInLaborForce + nMaleNotEnrollNoGradNotInLaborForce +
      nFemaleNotEnrollHSGradNotInLaborForce + nFemaleNotEnrollNoGradNotInLaborForce) / n16to19 * 100
  ) %>% 
  select(-matches('edTotal|^nMale|^nFemale|^n16'))
get_str(dat)
map(dat, range, na.rm = TRUE)

# Pivot longer
dat <- dat %>% 
  pivot_longer(
    cols = !c(year, fips),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(dat)



## Metadata ----------------------------------------------------------------


meta_vars(dat)

# Load from CSV
meta <- read_csv('5_objects/metadata_csv/census_meta.csv')
get_str(meta)

meta <- meta %>% 
  arrange(variable_name) %>% 
  mutate(
    resolution = meta_resolution(dat),
    updates = '5 years',
    latest_year = meta_latest_year(dat),
    year = meta_years(dat)
  ) %>% 
  meta_citation(date = '2025-07-03')
get_str(meta)



# Save and Clear ----------------------------------------------------------


# Check to make sure we have the same number of metrics and metas
check_n_records(dat, meta, 'Census')

# Save them
saveRDS(dat, '5_objects/metrics/census.RDS')
saveRDS(meta, '5_objects/metadata/census_meta.RDS')

clear_data(gc = TRUE)
