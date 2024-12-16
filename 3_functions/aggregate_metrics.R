# Aggregate and save results and metas
# For use at end of data wrangling scripts
# Takes a list of metadata DFs and a list of results DFs in long format

aggregate_metrics <- function(metrics = results,
                              metadata = metas) {
  result <<- map(metrics, ~ {
    .x %>%
      mutate(
        value = as.character(value), 
        year = as.character(year)
      )
  }) %>% 
    bind_rows()
  
  meta <<- map(metas, ~ {
    .x %>% 
      mutate(latest_year = as.character(latest_year))
  }) %>%  
    bind_rows()
}
