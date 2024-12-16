# Test out FDA API


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
  base_get,
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


# Do metadata here []