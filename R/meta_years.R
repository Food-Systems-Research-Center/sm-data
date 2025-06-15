#' Metadata: Get all years per variable
#' 
#' @description
#' Get a sorted vector of years for each variable_name from a metrics-style
#' long-format df. To be used in metadata creation.
#' 
#' @param df A long-format metrics df.
#'
#' @returns Sorted vector of years for each variable_name. Variable_name is also
#' returned in sorted order.
#' 
#' @export
meta_years <- function(df) {
  variable <- sort(as.character(unique(df$variable_name)))
  out <- map_chr(variable, ~ {
    df %>% 
      filter(variable_name == .x) %>% 
      pull(year) %>% 
      unique() %>% 
      sort() %>% 
      paste0(collapse = ', ')
  })
  return(out)  
}
