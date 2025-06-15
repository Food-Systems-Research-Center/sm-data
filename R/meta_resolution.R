#' Metadata: Get resolution per variable
#'
#' @description Get a vector of resolution, one for each variable_name from a
#'   metrics-style long-format df. To be used in metadata creation.
#'
#' @param df A long-format metrics df.
#'
#' @returns Vector of resolutions ("state", "county", "county, state"), one for
#'   each variable, in order of variable names.
#' @export
meta_resolution <- function(df) {
  out <- df %>% 
    group_by(variable_name) %>% 
    summarize(
      resolution = case_when(
        any(str_length(fips) == 2) & any(str_length(fips) == 5) ~ 'county, state',
        all(str_length(fips) == 2) ~ 'state',
        all(str_length(fips) == 5) ~ 'county'
    )) %>% 
    arrange(variable_name) %>% 
    pull(resolution)
  return(out)
}
