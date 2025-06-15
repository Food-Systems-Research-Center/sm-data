#' Read all RDS files at a given location
#'
#' @description
#' For internal use only, really just in the aggregation and export stages.
#' 
#' @param path File path from which to load RDS files
#' @param pattern regex matching pattern
#' @param split What character to split on to separate file suffix from name
#'
#' @returns List of RDS files
#' @importFrom stringr str_split_i
#' @importFrom purrr map
#' @export
read_all_rds <- function(path, pattern = NULL, split = '\\.') {
  paths <- list.files(path, pattern = pattern, full.names = TRUE)
  names <- list.files(path, pattern = pattern, full.names = FALSE) %>% 
    str_split_i(split, 1)
  out <- map(paths, readRDS) %>% 
    setNames(c(names))
  return(out)
}
