# County Health Ranking dataset
# 2024-12-13

# https://www.countyhealthrankings.org/


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  readxl,
  purrr,
  snakecase,
  tidyr,
  readr,
  stringr
)

source('3_functions/metadata_utilities.R')

# Load fips key to filter to NE counties
fips_key <- readRDS('5_objects/fips_key.rds')
state_key <- readRDS('5_objects/state_key.rds')

# Initiate results and metas lists
res <- list()
metas <- list()



# Outcomes and Factors ----------------------------------------------------


# Load national data with health factors and health outcomes
# Note that national Z-scores are only available when downloading by state
# Because they really want to make it difficult for some reason
path <- '1_raw/county_health_rankings/national/2024_county_health_release_data_-_v1.xlsx'
sheets <- excel_sheets(path)
nat_raw <- read_xlsx(path, sheet = sheets[6], skip = 1)

# Clean up names, removes states, filter to NE counties
nat <- nat_raw %>% 
  select(-c(State, County, starts_with('Number of'))) %>% 
  setNames(c(
    'fips',
    'healthOutcomeZ',
    'healthOutcomeGroup',
    'healthFactorZ',
    'healthFactorGroup'
  )) %>% 
  mutate(
    healthOutcomeGroup = case_when(
      str_detect(healthOutcomeGroup, '^-1.76') ~ 10,
      str_detect(healthOutcomeGroup, '^-1.09') ~ 9,
      str_detect(healthOutcomeGroup, '^-0.72') ~ 8,
      str_detect(healthOutcomeGroup, '^-0.4') ~ 7,
      str_detect(healthOutcomeGroup, '^-0.1') ~ 6,
      str_detect(healthOutcomeGroup, '^0.22') ~ 5,
      str_detect(healthOutcomeGroup, '^0.56') ~ 4,
      str_detect(healthOutcomeGroup, '^0.95') ~ 3,
      str_detect(healthOutcomeGroup, '^1.42') ~ 2,
      str_detect(healthOutcomeGroup, '^2.02') ~ 1,
      .default = NA
    ),
    healthFactorGroup = case_when(
      str_detect(healthFactorGroup, '^-1.62') ~ 10,
      str_detect(healthFactorGroup, '^-0.96') ~ 9,
      str_detect(healthFactorGroup, '^-0.67') ~ 8,
      str_detect(healthFactorGroup, '^-0.44') ~ 7,
      str_detect(healthFactorGroup, '^-0.22') ~ 6,
      str_detect(healthFactorGroup, '^0 to') ~  5,
      str_detect(healthFactorGroup, '^0.23') ~  4,
      str_detect(healthFactorGroup, '^0.47') ~  3,
      str_detect(healthFactorGroup, '^0.75') ~  2,
      str_detect(healthFactorGroup, '^1.12') ~  1,
      .default = NA
    ),
    across(ends_with('Z'), ~ -1 * .x)
  ) %>% 
  filter(fips %in% fips_key$fips | str_detect(fips, '000$'))
get_str(nat)

# Check it out
# library(sf)
# library(mapview)
# counties <- readRDS('2_clean/spatial/ne_counties_2021.RDS')
# counties <- left_join(counties, nat)
# mapview(counties, zcol = 'healthOutcomeZ')
# mapview(counties, zcol = 'healthOutcomeGroup')
# mapview(counties, zcol = 'healthFactorZ')
# mapview(counties, zcol = 'healthFactorGroup')
# Note that we recoded it so that higher z values are better. Originally, they
# have it so low z values and low groups are better. Seems to cap just above 0

# Convert to variable format
get_str(nat)
nat <- nat %>% 
  pivot_longer(
    cols = !fips,
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  mutate(year = '2024')
get_str(nat)

# Save to results
res$nat <- nat



## Metadata ----------------------------------------------------------------


(vars <- nat$variable_name %>% unique %>% sort)

metas$nat <- data.frame(
  variable_name = vars,
  definition = c(
    'National grouping of counties based on health factors index (30% health behaviors, 20% clinical care, 40% social and economic factors, 10% physical environment). Counties are split into 10 groups based on k-means clustering of z-scores and demographic data. Values are recoded such that larger numbers represent healthier counties.',
    'Nationally standardized z-score index of county level health factors (30% health behaviors, 20% clinical care, 40% social and economic factors, 10% physical environment). Scores are recoded so that larger values represent healthier counties.',
    'National grouping of counties based on health outcomes index (50% premature death rate, 10% reports of poor or fair health, 10% reports of poor physical health days, 10% reports of poor mental health days, 10% low birthweight rates). Counties are split into 10 groups based on k-means clustering of z-scores and demographic data. Values are recoded such that larger numbers represent healthier counties.',
    'Nationally standardized z-score index of county level health outcomes (50% premature death rate, 10% reports of poor or fair health, 10% reports of poor physical health days, 10% reports of poor mental health days, 10% low birthweight rates). Scores are recoded so that larger values represent healthier counties.'
  ),
  metric = c(
    'Health Factor Group',
    'Health Factor Z-Score',
    'Health Outcome Group',
    'Health Outcome Z-Score'
  ),
  dimension = "health",
  index = 'physical health',
  indicator = 'county health rankings',
  units = c(
    'categorical',
    'index',
    'categorical',
    'index'
  ),
  scope = 'national',
  resolution = 'county',
  year = '2024',
  latest_year = '2024',
  updates = "annual",
  source = 'University of Wisconsin Population Health Institute, County Health Rankings and Roadmaps. (2024)',
  url = 'https://www.countyhealthrankings.org/health-data/methodology-and-sources/data-documentation'
) %>%
  mutate(axis_name = metric) %>% 
  add_citation(access_date = '2024-12-13')

metas$nat



# Select Measures ---------------------------------------------------------


# Also pulling other interesting measures from national data. Note that we can
# pull county, state, and US data here. Using data dictionary to find the
# variables we want from the analytic sheets
dict <- read_xlsx('1_raw/county_health_rankings/DataDictionary_2024.xlsx') %>% 
  mutate(Measure = str_replace_all(Measure, '\n', ' '))

# All variables look good, just take them all. Get just var code for now
dict_summary <- dict %>% 
  filter(
    str_detect(Measure, 'raw value$'),
    str_detect(Measure, '%', negate = TRUE)
  )
var_codes <- dict_summary %>% 
  pull(`Variable Name`) %>% 
  str_split_i('_', 1)
suffixes <- c('_rawvalue', '_cilow', '_cihigh')
vars <- paste0(rep(var_codes, each = length(suffixes)), suffixes)

# Pull last 5 years of analytic data
paths <- dir('1_raw/county_health_rankings/analytic/', full.names = TRUE)
analytic <- map(paths, ~ read_csv(.x, skip = 1))

# Check it
get_str(analytic, 3)
head(analytic[[1]][, 1:4])

# Clean them as individual DFs, take vars we want, then see if we can bind them
select_measures <- map(analytic, ~ {
  .x %>% 
    select(statecode, fipscode, year, any_of(vars)) %>% 
    filter(
      statecode %in% fips_key$fips 
       | fipscode %in% fips_key$fips
       | fipscode %in% state_key$full_state_code
    ) %>% 
    setNames(c(
      ifelse(
        names(.) %in% dict$`Variable Name`,
        dict$Measure[match(names(.), dict$`Variable Name`)],
        names(.)
      )
    )) %>% 
    select(-'State FIPS Code') %>%
    rename(
      fips = '5-digit FIPS Code',
      year = 'Release Year'
    ) %>% 
    mutate(fips = case_when(
      fips == '00000' ~ '00',
      str_detect(fips, '000$') ~ str_remove(fips, '000'),
      .default = fips
    )) %>% 
    pivot_longer(
      cols = !c(fips, year),
      names_to = 'variable_name',
      values_to = 'value'
    ) 
    # mutate(variable_name = str_remove(variable_name, 'RawValue'))
}) %>% 
  bind_rows()
get_str(select_measures)

# Remove any NAs
# Save to results
res$select_measures <- select_measures



## Metadata ----------------------------------------------------------------


vars <- res$select_measures$variable_name %>% 
  unique %>% 
  sort
vars
dict

# Get set of names and definitions we actually used
select_meta <- dict %>% 
  select(-`Variable Name`) %>% 
  filter(Measure %in% vars)

for (row in 1:nrow(select_meta)) {
  if (str_detect(select_meta$Measure[row], ' raw value$')) {
    select_meta$Measure[row] <- str_remove(select_meta$Measure[row], ' raw value')
  } else if (str_detect(select_meta$Measure[row], ' CI low$')) {
    select_meta$Description[row] <- paste0(
      'Confidence Interval, Low: ',
      select_meta$Description[row - 1]
    )
  } else if (str_detect(select_meta$Measure[row], ' CI high$')) {
    select_meta$Description[row] <- paste0(
      'Confidence Interval, High: ',
      select_meta$Description[row - 1]
    )
  }
}
get_str(select_meta)
select_meta

# Reference measures
ref_measures <- res$select_measures %>% 
  filter(str_detect(variable_name, ' CI ', negate = TRUE)) %>% 
  mutate(variable_name = str_remove(variable_name, ' raw value')) %>% 
  select(-fips, -value) %>% 
  unique()

# Start putting it into metadata format, but stop at dimensions
select_meta_meta <- select_meta %>% 
  rename(definition = Description) %>% 
  filter(str_detect(Measure, ' CI low$| CI high$', negate = TRUE)) %>% 
  mutate(
    metric = str_to_sentence(Measure),
    variable_name = snakecase::to_lower_camel_case(Measure),
    axis_name = variable_name, # Could try to give better names...
    dimension = case_when(
      str_detect(metric, regex('social|disconnected|census|turnout|traffic|arrest|segregation|commute|driving|single-parent', ignore_case = TRUE)) ~ 'social',
      str_detect(metric, regex('broadband|poverty|employm|income|pay gap|free|wage', ignore_case = TRUE)) ~ 'economics',
      str_detect(metric, regex('population', ignore_case = TRUE)) ~ 'utilities',
      .default = 'health'
    ),
    scope = 'national',
    resolution = 'county, state',
    year = paste0(2020:2024, collapse = ', '),
    latest_year = '2024',
    updates = "annual",
    source = 'University of Wisconsin Population Health Institute, County Health Rankings and Roadmaps. (2024)',
    url = 'https://www.countyhealthrankings.org/health-data/methodology-and-sources/data-documentation'
  ) %>% 
  add_citation(access_date = '2024-12-13') %>% 
  select(-Measure)

select_meta_meta %>% select(1, 5)
select_meta_meta
select_meta_meta$variable_name %>% unique

# Now just deal with social
metas$select_measures_social <- select_meta_meta %>%
  filter(dimension == 'social') %>% 
  arrange(variable_name) %>% 
  mutate(
    index = c(
      'community livability',
      'food system governance',
      'community embeddedness',
      'community embeddedness',
      'community embeddedness',
      'community livability',
      'community livability',
      'community livability',
      'community livability',
      'community embeddedness',
      'community livability',
      'food system governance'
    ),
    indicator = c(
      'community safety',
      'participatory governance',
      'social connectedness',
      'social connectedness',
      'tbd',
      'community safety',
      'tbd',
      'diverse representation',
      'diverse representation',
      'social connectedness',
      'tbd',
      'participatory governance'
    ),
    units = c(
      rep('percentage', 5),
      'delinquencies per 1,000 juvelines',
      'percentage',
      'index',
      'index',
      'associations per 10,000 residents',
      'traffic volume per peter of roadway',
      'percentage'
    )
  )

# Economics
metas$select_measures_economics <- select_meta_meta %>%
  filter(dimension == 'economics') %>% 
  arrange(variable_name) %>% 
  mutate(
    index = 'community economy',
    indicator = c(
      'tbd',
      rep('wealth/income distribution', 6),
      'unemployment'
    ),
    units = c(
      rep('percentage', 3),
      'ratio',
      'ratio',
      'usd/hour',
      'usd',
      'percentage'
    )
  )

# Health
metas$select_measures_health <- select_meta_meta %>%
  filter(dimension == 'health') %>% 
  arrange(variable_name) %>% 
  mutate(
    index = c(
      'health infrastructure',
      rep('physical health', 3),
      rep('health infrastructure', 2),
      'physical health',
      'health infrastructure',
      'physical health',
      'health infrastructure',
      rep('physical health', 4),
      'health infrastructure',
      'food security',
      'mental health',
      'physical health',
      rep('education', 2),
      'physical health',
      'housing',
      'physical health',
      rep('physical health', 4),
      'health infrastructure',
      'physical health',
      'health infrastructure',
      'education',
      'health infrastructure',
      'physical health',
      'health infrastructure',
      'physical health',
      'mental health',
      rep('physical health', 5),
      'health infrastructure',
      'education',
      'education',
      'housing',
      'housing',
      'physical health',
      'education',
      'mental health',
      'physical health',
      rep('health infrastructure', 3)
    ),
    indicator = 'tbd',
    units = case_when(
      str_detect(definition, 'Percentage|percent') ~ 'percentage',
      str_detect(definition, ' per ') ~ 'ratio',
      str_detect(definition, 'Index') ~ 'index',
      str_detect(definition, '1=Yes') ~ 'binary',
      str_detect(definition, 'years|Average|average') ~ 'numeric'
    )
  )

# Remove the initial set of metadata from results list
metas$select_measures <- NULL

# Now we can also make our metric variable names formatted properly
res$select_measures$variable_name <- res$select_measures$variable_name %>% 
  snakecase::to_lower_camel_case() %>% 
  str_remove('RawValue')



# Aggregate and Save ------------------------------------------------------


# Combine lists together, one for results, one for meta
out <- aggregate_metrics(res, metas)

# Check record counts
try(check_n_records(out$result, out$meta, 'County Health'))
# This is cool - we didn't count the confidence intervals as metrics

saveRDS(out$result, '5_objects/metrics/county_health.RDS')
saveRDS(out$meta, '5_objects/metadata/county_health.RDS')

clear_data()
gc()

