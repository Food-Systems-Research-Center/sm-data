# Convenience functions for writing metadata

pacman::p_load(
  dplyr,
  purrr,
  lubridate
)
# Get unique variable_names from a long-format DF
get_vars <- function(df, col_name = 'variable_name') {
  df[[col_name]] %>% 
    unique %>% 
    sort
}

# Get a series of all years for each variable_name, with commas in between
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

# Get latest year for each variable_name in df
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

# Get resolution (state, county, state and county) for each variable
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

# Create a citation column on metadata with source, url, and access date
add_citation <- function(df, 
                         source_col = source, 
                         url_col = url,
                         access_date = NULL) {
  if (is.null(access_date)) {
    date_of_access <- format(Sys.Date(), "%B %d, %Y")
  } else {
    date_of_access <- format(ymd(access_date), "%B %d, %Y")
  }
  
  df %>%
    mutate(citation = paste0(
      {{ source_col }},
      # '. ',
      ' Retrieved from ',
      {{ url_col }},
      ', accessed on ',
      date_of_access,
      '.'
    ))
}

# Make this better
add_citation_2 <- function(df, 
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

# Jenky af - map this over onto add_citation for now
add_citation <- add_citation_2


# Make sure the number of variables in results and variables in metadata match
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

# Comebine results and metas into result and meta
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
