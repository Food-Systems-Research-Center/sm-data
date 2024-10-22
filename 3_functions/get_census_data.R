# get census data
# 2024-10-12

#' Use census API to pull census data. Note that only one year can apparently
#' be pulled at a time, but you can map over a list of years to gather many
#' at once.

pacman::p_load(
  dplyr,
  httr,
  jsonlite,
  glue,
  purrr,
  stringr,
  janitor
)

get_census_data <- function(survey_year,
                            survey,
                            vars,
                            county = '*',
                            state) {
  # Get URL
  url <- glue(
    'https://api.census.gov/data/{survey_year}/{survey}',
    '?get=GEO_ID,NAME,{vars}',
    '&for=county:{county}',
    '&in=state:{state}'
  )
  
  # Print to check
  cat(paste0('\nQuery:\n', url, '\n'))
  
  # Make request with error handling
  out <- tryCatch({
    
    # Make request    
    response <- GET(url)
    
    # Check if the status code is not 200 (success)
    if (http_status(response)$category != "Success") {
      cat("\nFailed request with status: ", http_status(response)$message)
      return(
        list(
          status = http_status(response)$message,
          query = url
        )
      )
    }
    
    # If success, wrangle data
    content(response, as = 'text') %>%
      fromJSON() %>%
      as.data.frame() %>%
      # Var names come out in first row. Make them column names
      row_to_names(1) %>%
      mutate(
        # Create 5 digit fips code from state and county codes
        fips = paste0(state, county),
        # Add survey year as column because it is not recorded anywhere else
        year = survey_year
      )
  }, error = function(e) {
    # Error message in case of failure
    cat("Error: ", e$message, "\n")
    return(
      list(
        error_message = e$message,
        query = url
      )
    )
  })
  
  return(out)
}



# Improved ----------------------------------------------------------------


get_census_data_2 <- function(survey_year,
                            survey,
                            vars,
                            county = '*',
                            state) {
  
  
  # Get URL
  url <- glue(
    'https://api.census.gov/data/{survey_year}/{survey}',
    '?get=GEO_ID,NAME,{vars}',
    '&for=county:{county}',
    '&in=state:{state}'
  )
  
  # Print to check
  cat(paste0('\nQuery:\n', url, '\n'))
  
  # Make request with error handling
  out <- tryCatch({
    
    # Make request    
    response <- GET(url)
    
    # Check if the status code is not 200 (success)
    if (http_status(response)$category != "Success") {
      cat("\nFailed request with status: ", http_status(response)$message)
      return(
        list(
          status = http_status(response)$message,
          query = url
        )
      )
    }
    
    # If success, wrangle data
    content(response, as = 'text') %>%
      fromJSON() %>%
      as.data.frame() %>%
      # Var names come out in first row. Make them column names
      row_to_names(1) %>%
      mutate(
        # Create 5 digit fips code from state and county codes
        fips = paste0(state, county),
        # Add survey year as column because it is not recorded anywhere else
        year = survey_year
      )
  }, error = function(e) {
    # Error message in case of failure
    cat("Error: ", e$message, "\n")
    return(
      list(
        error_message = e$message,
        query = url
      )
    )
  })
  
  return(out)
}
