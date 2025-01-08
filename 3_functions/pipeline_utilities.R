# Data Pipeline Functions
# 2024-10-04

# Several functions to help in wrangle aggregated long data and prepare for 
# mapping and analysis.

pacman::p_load(
  purrr,
  dplyr,
  stringr,
  purrr,
  rlang
)

# Pivot wider and add year to the variable name
make_wider <- function(df, 
                       var_col = 'variable_name', 
                       year_col = 'year', 
                       val_col = 'value') {
  out <- df %>% 
    mutate(
      {{ var_col }} := paste0(.data[[var_col]], '_', .data[[year_col]]),
      .keep = 'unused'
    ) %>% 
    pivot_wider(
      names_from = {{ var_col }},
      values_from = {{ val_col }}
    ) %>% 
    mutate(across(2:ncol(.), as.numeric))
}

# Filter a long-format dataset by fips by certain preset categories
fips_key <- readRDS('5_objects/fips_key.rds')
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

# Read all RDS files in a folder, by pattern
read_all_rds <- function(path, pattern = NULL, split = '\\.') {
  paths <- list.files(path, pattern = pattern, full.names = TRUE)
  names <- list.files(path, pattern = pattern, full.names = FALSE) %>% 
    str_split_i(split, 1)
  out <- map(paths, readRDS) %>% 
    setNames(c(names))
  return(out)
}
