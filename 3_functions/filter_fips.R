#' Function to filter fips to five options:
#' 1. 'all' All NE counties (both CT counties and CT regions) and states
#' 2. 'counties' All NE counties (both CT counties and regions)
#' 2. 'new' 2024 New England counties (CT regions)
#' 3. 'old' 2021 New England counties (CT counties)
#' 4. 'states' all New England states
#' 5. 'us' aggregate US data only



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  rlang,
  stringr
)

fips_key <- readRDS('5_objects/fips_key.rds')



# Function ----------------------------------------------------------------


filter_fips <- function(df, 
                        scope = c('all', 'counties', 'new', 'old', 'states', 'us'),
                        fips_col = 'fips') {
  
  # Match to one of arguments if it is a short version
  scope <- match.arg(scope)
  
  # Filter to set of fips numbers based on scope
  if (scope == 'all') {
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% fips_key$fips)
  } else if (scope == 'counties') {
    subset <- fips_key %>% 
      dplyr::filter(str_length(fips) == 5) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
  } else if (scope == 'new') {
    subset <- fips_key %>% 
      dplyr::filter(
        str_length(fips) == 5,
        !str_detect(fips, '^09.*[1-9]$')
      ) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
    
  } else if (scope == 'old') {
    subset <- fips_key %>% 
      dplyr::filter(
        str_length(fips) == 5,
        !str_detect(fips, '^09.*0$')
      ) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
    
  } else if (scope == 'states') {
    subset <- fips_key %>% 
      dplyr::filter(
        str_length(fips) == 2,
        is.na(county_name),
        state_name != 'US'
      ) %>% 
      pull(fips)
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] %in% subset)
    
  } else if (scope == 'us') {
    out <- df %>% 
      dplyr::filter(.data[[fips_col]] == '00')
    
  } else {
    stop('Could not filter fips.')
  }
  
  return(out)  
}

