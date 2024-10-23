# BLS QCEW
# 2024-10-23

# Pulling QCEW data from BLS. This is source of NAICS labor data in FAME warehouse



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  blsAPI,
  jsonlite,
  httr,
  glue,
  snakecase
)

source('3_functions/add_citation.R')
source('3_functions/check_n_records.R')
fips_key <- readRDS('5_objects/fips_key.rds')
naics_key <- readRDS('5_objects/naics_key.rds')

# Results list
results <- list()



# Pull All Counties -------------------------------------------------------


# Try mapping over several counties
out <- map(fips_key$fips, \(fips) {
  
  tryCatch({
    blsQCEW(
      method = 'Area',
      year = '2023',
      quarter = 'a',
      area = fips
    )
  }, error = function(e) {
    cat("Error: ", e$message, "\n")
    return(
      list(
        error_message = e$message,
        fips = fips
      )
    )
  })
})

get_str(out)
# Works!

# Save this as intermediate object so we don't have to call API
saveRDS(out, '5_objects/api_outs/bls_qcew.rds')



# Clean -------------------------------------------------------------------


out <- readRDS('5_objects/api_outs/bls_qcew.rds')

# Bind, then filter to relevant industries
dat <- out %>% 
  keep(is.data.frame) %>% 
  bind_rows() %>% 
  mutate(
    area_fips = as.character(area_fips),
    area_fips = ifelse(
      str_length(area_fips) == 4, 
      paste0('0', area_fips), 
      area_fips
    )
  ) %>% 
  filter(industry_code %in% naics_key$naics)
get_str(dat)

# Remove irrelevant columns
dat <- dat %>% 
  select(-c(
    own_code, agglvl_code, size_code, qtr
  )) %>% 
  unite(
    col = 'disclosure',
    matches('disclosure'),
    sep = ''
  ) %>% 
  pivot_longer(
    cols = !c('area_fips', 'industry_code', 'year', 'disclosure'),
    names_to = 'variable_name',
    values_to = 'value'
  ) %>%
  mutate(
    disclosure = ifelse(disclosure == '', NA, 'N'),
    variable_name = paste0(
      snakecase::to_lower_camel_case(variable_name),
      'NAICS',
      industry_code
    ),
    .keep = 'unused'
  ) %>% 
  rename(fips = area_fips)
get_str(dat)

dat$variable_name %>% unique %>% sort


# Metadata ----------------------------------------------------------------


(vars <- dat$variable_name %>% unique %>% sort)

# Join qcew data to the NAICS key
qcew_fields <- read_csv('1_raw/bls/naics-based-annual-layout.csv')
meta <- dat %>% 
  mutate(
    variable_name = str_remove_all(variable_name, 'NAICS[0-9]*') %>% 
      snakecase::to_snake_case()
  ) %>% 
  select(variable_name) %>% 
  unique() %>% 
  left_join(qcew_fields, by = join_by(variable_name == field_name)) %>% 
  select(
    variable_name,
    definition = field_description
  ) %>% 
  mutate(
    variable_name = snakecase::to_lower_camel_case(variable_name)
  )

meta <- meta %>% 
  mutate(
    dimension = 'economics',
    index = 'community economy',
    indicator = case_when(
      str_detect(variable_name, 'Emplvl') ~ 'job availability',
      str_detect(variable_name, 'Estabs') ~ 'business failure rate',
      str_detect(variable_name, 'Wage|Pay') ~ 'wage rate',
      .default = NA
    ),
    axis_name = variable_name, # fix this eventually...
    metric = variable_name, # fix this eventually...
    units = case_when(
      str_detect(variable_name, 'Estabs') ~ 'ratio',
      str_detect(variable_name, 'Emplvl') ~ 'count',
      str_detect(variable_name, 'Wage|AnnualPay') ~ 'usd',
      str_detect(variable_name, '^lq') ~ 'ratio',
      str_detect(variable_name, '^oty') ~ 'percentage',
    ),
    annotation = 'disclosure',
    scope = 'national',
    resolution = 'county',
    year = '2023',
    updates = 'annual',
    warehouse = FALSE,
    source = 'U.S. Bureau of Labor Statistics, Quarterly Census of Employment and Wages (2023)',
    url = 'https://www.bls.gov/cew/'
  ) %>% 
  add_citation()
  
get_str(meta)



# Regular -----------------------------------------------------------------


# seriesid='BDS0000000000300115110001LQ5'
# 
# url <- glue(
#   'https://api.bls.gov/publicAPI/v2/timeseries/data/',
#   '{seriesid}'
# )
# 
# response <- GET(url, add_headers('Content-Type=application/x-www-form-urlencoded'))
# get_str(response)
# 
# out <- content(response, as = 'text') %>% 
#   fromJSON()
# get_str(out$Results$series$data[[1]])
# # This works too if we know series ID up front



# Save and Clear ----------------------------------------------------------


check_n_records(dat, meta, 'BLS')

get_str(dat)
get_str(meta)

saveRDS(dat, '5_objects/metrics/bls.RDS')
saveRDS(meta, '5_objects/metadata/bls_meta.RDS')
