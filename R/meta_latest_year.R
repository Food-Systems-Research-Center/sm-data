#' Metadata: Get latest year per variable
#' 
#' @description
#' Get a vector of latest years, one for each variable_name from a metrics-style
#' long-format df. To be used in metadata creation.
#'
#' @param df A long-format metrics df. 
#'
#' @returns Vector of years, one for each variable, in order of variable names
#' @export
meta_latest_year <- function(df) {
  variable <- sort(as.character(unique(df$variable_name)))
  out <- map_chr(variable, ~ {
    df %>% 
      filter(variable_name == .x) %>% 
      pull(year) %>% 
      as.character() %>% 
      unique() %>% 
      max()
  })
  return(out)
}
