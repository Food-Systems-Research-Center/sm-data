# get_usdm

get_usdm <- function(base,
                     drought_categories,
                     states,
                     start_date,
                     end_date) {
  
  out <- map(states, \(state) {
    state_out <- map(drought_categories, \(cat) {
      
      # Build query based on parameters
      url <- glue(
        base,
        'GetNonConsecutiveStatisticsCounty?',
        'aoi={state}',
        '&dx={cat}',
        '&startdate={start_date}',
        '&enddate={end_date}'
      )
      print(url)
      
      # Get all categories for given state
      cat_out <- GET(url, add_headers(Accept = "text/json")) %>%
        content(as = 'text') %>%
        fromJSON()
      
      if (length(cat_out) > 0) {
        cat_out <- cat_out %>% 
          as.data.frame() %>% 
          select(fips, nonConsecutiveWeeks)
      }
      
      return(cat_out)
    }) %>% 
      setNames(c(paste0('cat_', drought_categories)))
  }) %>% 
    setNames(c(states))
  return(out)
}