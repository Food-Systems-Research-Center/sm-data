#' Metadata Utilities
#'
#' @description Functions to build metadata
#'
#' - `aggregate_metrics()`: Input a list of metrics dfs and a list of metadata
#' dfs and return a list of two aggregated dfs, one for metrics and one for 
#' metadata. Use at end of wrangling script before `check_n_records()`.
#' - `check_n_records()`: Input a metric df and metadata df along with the 
#' section or script the function appears in and function will check to see 
#' whether all the metrics in the metric df match those in the metadata df. If 
#' they are not the same names and same counts, throws an error. Use at end of
#' each wrangling script.
#'
#' @import dplyr
#' @import purrr
#' @import stringr
#' @keywords internal
#' @rdname metadata_utilities
aggregate_metrics <- function(metrics = results,
                              metadata = metas) {
  out <- list()

  out$result <- purrr::map(metrics, ~ {
    .x %>%
      dplyr::mutate(
        value = as.character(value), 
        year = as.character(year)
      )
  }) %>% 
    dplyr::bind_rows()
  
  out$meta <- purrr::map(metas, ~ {
    .x %>% 
      dplyr::mutate(
        year = as.character(year),
        latest_year = as.character(latest_year)
      )
  }) %>%  
    dplyr::bind_rows()
  
  return(out)
}

#' @rdname metadata_utilities
check_n_records <- function(metric_vars, 
                            meta_vars, 
                            section = 'Section',
                            var_col = 'variable_name') {
  
  # Get clean vectors of each, sorted to compare
  metrics <- sort(unique(metric_vars[[var_col]]))
  
  # If NAICS variables are included, remove the NAICS and numeric code
  # Otherwise there will be heaps of "different variables" and won't match meta
  if (any(stringr::str_detect(metrics, 'NAICS'))) {
    metrics <- stringr::str_remove_all(metrics, 'NAICS[0-9]*.*') %>% 
      unique()
  }
  
  meta <- sort(unique(meta_vars[[var_col]]))
  
  # Check if they are all the same
  if (all(metrics == meta) & length(metrics) == length(meta)) {
    cat('\n', section, ' variable check: PASS\n',
        'Number of metrics: ', length(metrics), '\n',
        'Number of metas: ', length(meta), '\n\n', 
        sep = '')
  } else {
    stop('\n', section, ' variable check: FAIL\n',
         'Number of metrics: ', length(metrics), '\n',
         'Number of metas: ', length(meta), '\n\n')
  }
}
