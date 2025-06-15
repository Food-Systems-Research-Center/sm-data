#' Make metrics df wider
#'
#' @description 
#' Pivots wider based on defaults used in metrics data. Automatically pastes the
#' year to the name of new variable columns (`foodInsecurity` in 2023 becomes
#' `foodInsecurity_2023`).
#' 
#' @param df `metrics` df in long format
#' @param var_col column name representing variable name
#' @param year_col column name representing year
#' @param val_col column name representing value
#'
#' @returns `metrics` df in wide format
#' @export
#'
#' @examples
#' metrics <- data.frame(
#'   fips = c(50001, 50002, 50003),
#'   year = 2023,
#'   variable_name = 'var',
#'   value = c(1, 2, 3)
#' )
#' make_wider(metrics)
make_wider <- function(df, 
                       var_col = 'variable_name', 
                       year_col = 'year', 
                       val_col = 'value') {
  out <- df %>% 
    dplyr::mutate(
      {{ var_col }} := paste0(.data[[var_col]], '_', .data[[year_col]]),
      .keep = 'unused'
    ) %>% 
    tidyr::pivot_wider(
      names_from = {{ var_col }},
      values_from = {{ val_col }}
    ) %>% 
    dplyr::mutate(across(2:ncol(.), as.numeric))
}
