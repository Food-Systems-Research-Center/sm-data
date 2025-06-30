# BLS API
# 2025-06-30

# Pulled API calls from bls script here. Still need to make sure they work
# properly



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  jsonlite,
  httr,
  glue,
  snakecase,
  readxl,
  tidyr,
  stringr,
  readr,
  purrr
)

pacman::p_load_gh('mikeasilva/blsAPI')

# Results list
results <- list()
metas <- list()



# QCEW --------------------------------------------------------------------


# Pull counties and states

# Get a list of NE counties and all US states
fips_list <- c(
  fips_key$fips[str_length(fips_key$fips) == 5],
  paste0(state_key$state_code, '000')
) %>% 
  unique()

# # Map over whole list. Just 2023 apparently for now
# out <- map(fips_list, \(fips) {
# 
#   tryCatch({
#     blsQCEW(
#       method = 'Area',
#       year = '2023',
#       quarter = 'a',
#       area = fips
#     )
#   }, error = function(e) {
#     cat("Error: ", e$message, "\n")
#     return(
#       list(
#         error_message = e$message,
#         fips = fips
#       )
#     )
#   })
# })
# 
# get_str(out)
# Works!

# # Save this as intermediate object so we don't have to call API
# saveRDS(out, '5_objects/api_outs/bls_qcew.rds')



# BLS API -----------------------------------------------------------------


# Without using blsAPI app, just regular GET requests
# Guess we didn't end up using this though

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
