#' Call Census API
#'
#' @description Call Census API by year. For multiple years, loop the function
#'   over a vector of years. Note that we still need to add a Sys.time() so that
#'   we don't get rate limited. Also, consider the `censusapi` package, which
#'   might have been a better way to do this from the beginning.
#' @param survey_year Year as integer. Note that ACS5 is conducted on 5-year
#'   cycle, last one in 2022.
#' @param survey String describing which Census survey to pull from (e.g.,
#'   'acs/acs5')
#' @param vars String containing one or more variable codes separated by commas
#'   (e.g., "B01003_001E,B15003_001R")
#' @param county String containing one or more county FIPS codes separated by
#'   commas (e.g., "50001,50002")
#' @param state String containing one or more state FIPS codes separated by
#'   commas (e.g., "50,25")
#'
#' @returns List of API outputs for the given year
#' @import  dplyr
#' @importFrom  jsonlite fromJSON
#' @import  glue
#' @import  stringr
#' @import  janitor
#' @keywords internal
#'
#' @examples
call_census_api <- function(survey_year,
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