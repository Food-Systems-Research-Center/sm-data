# NASS
# 2025-07-02


# Description -------------------------------------------------------------


# Taking our API calls from nass_api.R and combining them. Using the nass 
# api parameters to start metadata



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  readr,
  tidyr
)

# Load API outs
census <- readRDS('5_objects/api_outs/neast_nass_census_2002_2022.rds')
survey <- readRDS('5_objects/api_outs/neast_nass_survey_2002_2022.rds')

# Load nass api parameters to build metadata
nass_params <- read_csv('5_objects/api_parameters/nass_api_parameters.csv')



# Wrangle -----------------------------------------------------------------


bound <- bind_rows(census, survey)
get_str(bound)
bound %>% 
  filter(str_detect(short_desc, 'LABOR, HIRED - NUMBER OF WORKERS'))

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



# Metadata ----------------------------------------------------------------


get_str(bound)
get_str(nass_params)
(vars <- meta_vars(dat))

# Join nass params with variable names from dat to start metadata
meta <- nass_params %>% 
  filter(source_desc != 'SURVEY') %>% 
  select(-ends_with('desc'), -note) %>% 
  right_join(distinct(select(dat, short_desc, unit_desc, variable_name))) %>% 
  arrange(variable_name)
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
    across(c(metric, definition), ~ case_when(
      is.na(.x) & str_detect(variable_name, 'Yield') ~ snakecase::to_sentence_case(short_desc),
      .default = .x
    )),
    units = case_when(
      is.na(units) ~ snakecase::to_sentence_case(unit_desc),
      .default = units
    ),
    axis_name = case_when(
      is.na(axis_name) ~ variable_name,
      .default = axis_name
    )
  ) %>% 
  
  # Fresh calculated variables
  mutate(
    latest_year = meta_latest_year(dat),
    all_years = meta_years(dat),
    resolution = meta_resolution(dat),
    scope = 'national',
    updates = '5 years',
    source = paste0(
      "U.S. Department of Agriculture, National Agricultural Statistics Service. ",
      "(2024). 2022 Census of Agriculture."
    ),
    url = 'https://www.nass.usda.gov/Publications/AgCensus/2022/',
  ) %>% 
  meta_citation(date = '2025-07-02') %>% 
  select(-c(ends_with('desc')))
get_str(meta)



# Aggregate ---------------------------------------------------------------


# Check record counts
check_n_records(dat, meta, 'nass')

saveRDS(dat, '5_objects/metrics/nass.RDS')
saveRDS(meta, '5_objects/metadata/nass_meta.RDS')

clear_data(gc = TRUE)

