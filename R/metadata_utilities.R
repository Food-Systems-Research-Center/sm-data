#' Metadata Utilities
#'
#' @description Functions to build metadata
#'
#' - `get_vars()`: Input a long format metrics df and return a vector of unique,
#' sorted variable names.
#' - `get_all_years()`: Input a long format metrics df and return a character 
#' string of all unique years to put in metadata. Return a vector of strings if
#' there is more than one unique variable.
#' - `get_max_year()`: Input a long format metrics df and return a character
#' string with the latest year represented. Returns a vector of strings if
#' there is more than one unique variable.
#' - `get_resolution()`: Input a long format metrics df and return a character
#' string or vector of strings describing the resolution for each metric as
#' either county or state. Determined based on the format of the FIPS keys
#' for each variable.
#' - `add_citation()`: Input a metadata df and return the df with an extra
#' column that combines the source with either the URL or the API URL. If API,
#' adds the access date to the citation.
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
#' @import lubridate
#' @keywords internal
#' @rdname metadata_utilities
get_vars <- function(df, col_name = 'variable_name') {
  df[[col_name]] %>%
    unique %>%
    sort
}

#' @rdname metadata_utilities
get_all_years <- function(df) {
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

#' @rdname metadata_utilities
get_max_year <- function(df) {
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

#' @rdname metadata_utilities
get_resolution <- function(df) {
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

#' @rdname metadata_utilities
add_citation <- function(df, 
                           date = format(Sys.Date(), "%B %d, %Y"),
                           source_col = source, 
                           url_col = url,
                           api_url = NULL) {
  if (is.null(api_url)) {
    out <- df %>%
      mutate(citation = paste0(
        {{ source_col }},
        ', ',
        {{ url_col }},
        ', accessed on ',
        date,
        '.'
      ))
  } else {
    out <- df %>%
      mutate(citation = paste0(
        {{ source_col }},
        ', <',
        {{ api_url }},
        '>, accessed on ',
        date,
        '.'
      ))
  }
}

#' @rdname metadata_utilities
aggregate_metrics <- function(metrics = results,
                              metadata = metas) {
  out <- list()

  out$result <- map(metrics, ~ {
    .x %>%
      mutate(
        value = as.character(value), 
        year = as.character(year)
      )
  }) %>% 
    bind_rows()
  
  out$meta <- map(metas, ~ {
    .x %>% 
      mutate(
        year = as.character(year),
        latest_year = as.character(latest_year)
      )
  }) %>%  
    bind_rows()
  
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
  if (any(str_detect(metrics, 'NAICS'))) {
    metrics <- str_remove_all(metrics, 'NAICS[0-9]*.*') %>% 
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
