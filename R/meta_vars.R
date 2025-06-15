#' Metadata: Get variable names
#' 
#' @description
#' Get a sorted vector of variable names from a metrics-style long-format df. To
#' be used in metadata creation.
#'
#' @param df A long-format metrics df.
#' @param col_name Name of the column that contains the variable_name
#'
#' @returns Sorted vector of unique variable_name
#' @export
meta_vars <- function(df, col_name = 'variable_name') {
  df[[col_name]] %>% 
    unique %>% 
    sort
}