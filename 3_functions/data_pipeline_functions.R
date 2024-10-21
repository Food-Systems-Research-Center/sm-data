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


# Convenience function to filter so that each variable_name only has latest year
get_latest_year <- function(df, var_col = 'variable_name', year_col = 'year'){

  # Make sure that year is numeric
  df <- mutate(df, {{ year_col }} := as.numeric(.data[[year_col]]))
  
  # Get unique variable names
  vars <- unique(df[[var_col]])
  
  # Before 
  filtered_df <- map(vars, \(var) {
    unique_years <- df %>%
      dplyr::filter(.data[[var_col]] == var) %>%
      pull({{ year_col }}) %>% 
      unique()
    out <- df %>% 
      dplyr::filter(
        .data[[var_col]] == var, 
        .data[[year_col]] == max(unique_years)
      ) %>% 
      mutate({{ var_col }} := paste0(.data[[var_col]], '_', .data[[year_col]])) %>% 
      select(-{{ year_col }})
    return(out)
  }) %>% 
    bind_rows()
  
  return(filtered_df)
}

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


