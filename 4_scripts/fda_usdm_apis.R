# FDA API
# Pulling recall enforcement from FDA API and cleaning
# API call is commented out until necessary


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  readr,
  stringr
)

source('3_functions/aggregate_metrics.R')
source('3_functions/check_n_records.R')
source('3_functions/add_citation.R')

fips_key <- readRDS('5_objects/fips_key.rds')
results <- list()
metas <- list()



# Recall Enforcement ------------------------------------------------------


# +AND+ to string together multiple filters
# Capped at 1,000 per query, can page up to 26,000 using 'skip'
# Defaults to a single record if limit is not included
# Hard to get distribution pattern - language is not consistent. But can get
# origin.
# results as either meta and results or just error message

# Base endpoint
base <- 'https://api.fda.gov/food/enforcement.json?search='

# Glue together arguments
url <- glue(
  base,
  'state:(MA+OR+VT+OR+NH+OR+ME+OR+RI+OR+CT)',
  '+AND+report_date:[20190101+TO+20240101]',
  '&limit=1000'
)
url

## API call here - only run if needed

# out <- GET(url) %>%
#   content(as = 'text') %>%
#   fromJSON()
# 
# dat <- out$results
# get_str(dat)
# 
# # Save raw out file for posterity
# saveRDS(out, '5_objects/api_outs/fda_recalls_ne_2019_2024.rds')



# Wrangle -----------------------------------------------------------------


out <- readRDS('5_objects/api_outs/fda_recalls_ne_2019_2024.rds')
get_str(out)
get_str(out$results)

# Make sure we are in US and it is food
# Select relevant columns
# Note that we can pull it by zip code and by state, but we will need unique id
dat <- out$results %>% 
  filter(country == 'United States', product_type == 'Food') %>% 
  select(
    state, 
    postal_code, 
    recall_number, 
    date = recall_initiation_date
  ) %>% 
  mutate(
    zip = str_sub(postal_code, end = 5), 
    year = str_sub(date, end = 4),
    .keep = 'unused'
  )
get_str(dat)

# Swap out state for fips key
dat <- fips_key %>% 
  select(fips, state_code) %>% 
  right_join(dat, by = join_by(state_code == state)) %>% 
  select(-state_code)
get_str(dat)

# Here is where we split if we want to keep zip code
zip_level <- dat %>% 
  select(-fips) %>% 
  group_by(zip, year) %>% 
  summarize(nFoodRecallsZip = n()) %>% 
  pivot_longer(
    cols = nFoodRecallsZip,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(zip_level)

# Save it
results$food_recall_zip <- zip_level

# Now aggregate at state level
state_level <- dat %>% 
  select(-zip) %>% 
  group_by(fips, year) %>% 
  summarize(nFoodRecallsState = n()) %>% 
  pivot_longer(
    cols = nFoodRecallsState,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(state_level)

# Save it
results$food_recall_state <- state_level

# Let's just do all of new england while we're at it
ne_level <- dat %>% 
  select(-zip, -fips) %>% 
  group_by(year) %>% 
  summarize(nFoodRecallsNE = n()) %>% 
  pivot_longer(
    cols = nFoodRecallsNE,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(ne_level)

# Save it
results$food_recall_ne <- ne_level



## Metadata ----------------------------------------------------------------


# Check vars
vars <- map_chr(results, ~ {
  .x$variable_name %>% 
    unique
}) %>% 
  sort
vars

all_years <- map_chr(results, ~ {
  .x$year %>% 
    unique %>% 
    paste0(collapse = ', ')
}) %>% 
  sort

latest_years <- map_chr(results, ~ {
  .x$year %>%
    unique() %>% 
    str_split(', ') %>% 
    as.character() %>% 
    max()
}) %>% 
  unname()

metas$unemp <- data.frame(
  variable_name = vars,
  dimension = 'production',
  index = 'product quality',
  indicator = 'recalls in each industry',
  axis_name = c(
    'Food Recalls, New England',
    'Food Recalls by State',
    'Food Recalls by ZIP'
  ),
  metric = 'food recalls',
  definition = rep('Number of food recall enforcement reports documented in the FDA Recall Enterprise System', 3),
  units = 'count',
  annotation = NA,
  scope = 'national',
  resolution = c(
    'New England',
    'state',
    'ZIP code'
  ),
  year = all_years,
  latest_year = latest_years,
  updates = 'weekly',
  warehouse = FALSE,
  source = 'U.S. Food and Drug Administration, Recall Enterprise System (2024)',
  url = 'https://open.fda.gov/apis/food/enforcement/'
) %>% 
  add_citation(access_date = '2024-12-16')

get_str(metas$unemp)



# USDM --------------------------------------------------------------------


# Weeks of non-consecutive drought per year for each county in NE
# Drought categories 2 (severe) or higher, last 5 years
base <- 'https://usdmdataservices.unl.edu/api/ConsecutiveNonConsecutiveStatistics/'
drought_categories <- c(2, 3, 4)
states <- c('VT', 'NH', 'ME', 'MA', 'RI', 'CT')
start_dates <- c('1/1/2019', '1/1/2020', '1/1/2021', '1/1/2022', '1/1/2023')
end_dates <- c('12/31/2019', '12/31/2020', '12/31/2021', '12/31/2022', '12/31/2023')

## API Call - comment out until needed

# out <- map2(start_dates, end_dates, \(start_date, end_date){
#   get_usdm(
#     base = base,
#     drought_categories = drought_categories,
#     states = states,
#     start_date = start_date,
#     end_date = end_date
#   )
# }) %>%
#   setNames(c(paste0('y', 2019:2023)))
# 
# get_str(out)

# Save this for posterity
# saveRDS(out, '5_objects/api_outs/usdm_weeks_drought_2019_2023.rds')



## Cleaning ----------------------------------------------------------------


# Pull drought monitor data that was just saved
dm <- readRDS('5_objects/api_outs/usdm_weeks_drought_2019_2023.rds')
get_str(dm)
# Looks like no data for 2019 or 2023?

# Remove those years
dm <- dm[!names(dm) %in% c('y2019', 'y2023')]
get_str(dm, 4)

# Combine DFs
usdm <- imap(dm, \(year, year_name) {
  map(year, \(state) {
    
    not_null_dfs <- state %>% 
      keep(\(x) length(x) > 0)
    
    len <- length(not_null_dfs)
    if (len > 1) {
      out <- reduce(not_null_dfs, full_join)
    } else if (len == 1) {
      out <- not_null_dfs[[1]]
    } else {
      out <- NULL
    }
    return(out)
    
  }) %>% 
    keep(\(x) length(x) > 0) %>% 
    bind_rows() %>% 
    mutate(year = str_remove(year_name, 'y'))
}) %>% 
  bind_rows()
get_str(usdm)

# Mutate so we can weeks at 2 or more, and weeks at 3 or more
usdm[is.na(usdm)] <- 0
usdm <- usdm %>% 
  rename(
    droughtWeeksSevere = cat_2_weeks,
    droughtWeeksExtreme = cat_3_weeks
  ) %>% 
  mutate(
    droughtWeeksSevere = rowSums(select(., starts_with('drought')))
  ) %>% 
  pivot_longer(
    cols = starts_with('drought'),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(usdm)

# Make sure that all zeroes are accounted for, even implicit
grid <- expand.grid(
  fips = fips_key$fips[str_length(fips_key$fips) == 5 & 
    str_detect(fips_key$fips, '^09\\d{2}0$', negate = TRUE)],
  year = as.character(2020:2022),
  variable_name = c('droughtWeeksSevere', 'droughtWeeksExtreme')
)
usdm_clean <- full_join(grid, usdm)
usdm_clean[is.na(usdm_clean)] <- 0
get_str(usdm_clean)

# Save
results$usdm <- usdm_clean



## Metadata ----------------------------------------------------------------


vars <- usdm_clean$variable_name %>% 
  unique %>% 
  sort

inter <- usdm_clean %>% 
  group_by(variable_name) %>% 
  summarize(all_years = list(sort(unique(year)))) %>% 
  pull(all_years)

all_years <- map_chr(inter, ~ paste0(.x, collapse = ', '))
max_year <- map_chr(inter, max)

metas$usdm <- data.frame(
  variable_name = vars,
  dimension = 'environment',
  index = 'water stability',
  indicator = 'days / events of extremes',
  axis_name = c(
    'Weeks of Severe Drought',
    'Weeks of Extreme Drought'
  ),
  metric = c(
    'Weeks of severe drought',
    'Weeks of extreme drought'
  ),
  definition = c(
    'Weeks in the year in which there were severe droughts or worse, as defined by the US Drought Monitor (below 10th percentile for most indicators)',
    'Weeks in the year in which there were extreme droughts or worse, as defined by the US Drought Monitor (below 5th percentile for most indicators)'
  ),
  units = 'count',
  annotation = NA,
  scope = 'national',
  resolution = 'county',
  year = all_years,
  latest_year = max_year,
  updates = 'weekly',
  warehouse = FALSE,
  source = 'U.S. Department of Agriculture, U.S. Drought Monitor. (2024).',
  url = 'https://droughtmonitor.unl.edu/DmData/DataDownload.aspx'
) %>% 
  add_citation(access_date = '2024-12-17')

get_str(metas$usdm)



# Aggregate and Save ------------------------------------------------------


# Put metrics and metadata together into two single DFs
aggregate_metrics(results, metas)

# Check record counts to see if they match
check_n_records(result, meta, 'FSA, USDM')

# Save 
saveRDS(result, '5_objects/metrics/other.RDS')
saveRDS(meta, '5_objects/metadata/other_meta.RDS')

clear_data()
