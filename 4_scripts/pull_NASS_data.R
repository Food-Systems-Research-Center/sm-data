#' Pull NASS Data
#' 024-09-09

# Pulling data from USDA NASS for SM secondary data


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  memoise,
  purrr,
  readr
)

api_key <- Sys.getenv('NASS_KEY')
source('3_functions/api_functions.R')
par_options <- readRDS('projects/secondary_data/par_options.rds')



# VT Econ -----------------------------------------------------------------


base_get <- 'https://quickstats.nass.usda.gov/api/get_counts/'
base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'

# Parameters
source_desc <- 'survey'
sector_desc <- 'economics'
state_name <- 'Vermont'
year <- 2023
freq_desc <- 'annual'

# Glue it together
url <- glue(
  base_get,
  '?key={api_key}',
  '&state_name={state_name}',
  '&year={year}',
  '&freq_desc={freq_desc}',
  '&source_desc={source_desc}',
  '&sector_desc={sector_desc}'
)
url

out <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON() %>%
  .$data

get_str(out)

dat <- out %>% 
  pull_econ_cols() %>% 
  filter(state_name == 'VERMONT',
         domain_desc == 'TOTAL', 
         location_desc == 'VERMONT')
get_str(dat)
View(dat)




# NE Econ -----------------------------------------------------------------


# Map over vector of NE states
ne_states

base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'
base_get <- 'https://quickstats.nass.usda.gov/api/get_counts/'

# Parameters
source_desc <- 'survey'
sector_desc <- 'economics'
year <- 2023
freq_desc <- 'annual'

results <- map(ne_states, \(state_name) {
  url <- glue(
    base_get,
    '?key={api_key}',
    '&state_name={URLencode(state_name)}',
    '&year={year}',
    '&freq_desc={freq_desc}',
    '&source_desc={source_desc}',
    '&sector_desc={sector_desc}'
  )
  print(url)
  
  out <- GET(url) %>%
    content(as = 'text') %>%
    fromJSON() %>%
    .$data %>% 
    select(order(names(.)))
  return(out)
}) %>% 
  list_rbind()

get_str(results)

# Save this
saveRDS(results, 'projects/secondary_data/ne_econ.rds')



# VT Crops ----------------------------------------------------------------


base_get <- 'https://quickstats.nass.usda.gov/api/get_counts/'
base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'

# Parameters
state_name <- 'VERMONT'
year <- 2023
freq_desc <- 'annual'
source_desc <- 'survey'
sector_desc <- 'crops'

# Glue it together
url <- glue(
  base_get,
  '?key={api_key}',
  '&state_name={state_name}',
  '&year={year}',
  '&freq_desc={freq_desc}',
  '&source_desc={source_desc}',
  '&sector_desc={sector_desc}'
)
url

out <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON() %>%
  .$data
out
get_str(out)

dat <- out %>%
  lose_bad_cols()

# Save VT crops
saveRDS(out, 'projects/secondary_data/vt_crops.rds')

get_str(dat)



# VT Animal Products ------------------------------------------------------



base_get <- 'https://quickstats.nass.usda.gov/api/api_GET/'
base_get <- 'https://quickstats.nass.usda.gov/api/get_counts/'

# Parameters
state_name <- 'VERMONT'
year <- 2023
freq_desc <- 'annual'
source_desc <- 'survey'
sector_desc <- URLencode('animals & products')

# Glue it together
url <- glue(
  base_get,
  '?key={api_key}',
  '&state_name={state_name}',
  '&year={year}',
  '&freq_desc={freq_desc}',
  '&source_desc={source_desc}',
  '&sector_desc={sector_desc}'
)
url

out <- GET(url) %>%
  content(as = 'text') %>%
  fromJSON() %>%
  .$data


