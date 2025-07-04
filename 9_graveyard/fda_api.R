# FDA API
# 2025-06-30


# Description -------------------------------------------------------------

# Use FDA API to pull recall enforcement data
# Needs to be updated to cover whole Northeast

# NOTE: this is not actually used in secondary framework. Putting this on hold


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  readr,
  stringr,
  tidyr
)


# FDA Recall Enforcement ---------------------------------------------------


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

out <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON()

dat <- out$results
get_str(dat)

# Save raw out file
saveRDS(out, '5_objects/api_outs/fda_recalls_ne_2019_2024.rds')
clear_data()