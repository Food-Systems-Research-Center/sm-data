# USDM API
# 2025-06-30


# Description -------------------------------------------------------------

# Pull data from US Drought Monitor API


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


# Non-Consec Drought -------------------------------------------------------


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



# Avg Perc Drought by Area ------------------------------------------------


# Average weekly percent of each state that is in drought (d1 or greater)
states <- paste0(state_key$state_code, collapse = ',')
counties <- fips_key %>% 
  filter(str_length(fips) == 5) %>% 
  pull(fips) %>% 
  paste0(collapse = ',')
start_dates <- c(paste0('1/1/', 2020:2024))
end_dates <- c(paste0('12/31/', 2020:2024))

# Map over dates for last five years, get data by state
# out <- map2(start_dates, end_dates, \(start, end) {
#   url <- glue(
#     'https://usdmdataservices.unl.edu/api/StateStatistics/GetDroughtSeverityStatisticsByAreaPercent/',
#     '?aoi={states}',
#     '&startdate={start}',
#     '&enddate={end}',
#     '&statisticsType=1'
#   )
#   print(url)
#   out <- GET(url, add_headers(Accept = "text/json")) %>%
#     content(as = 'text') %>%
#     fromJSON()
#   return(out)
# }) %>% 
#   setNames(c(paste0('y', 2020:2024)))
# 
# get_str(out)

# Save output for posterity
# saveRDS(out, '5_objects/api_outs/usdm_perc_area_drought_2020_2024.rds')

