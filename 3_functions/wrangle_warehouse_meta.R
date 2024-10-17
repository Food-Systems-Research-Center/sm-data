wrangle_meta <- function(df, vars) {
  
  df %>% 
    select(
      variable_name,
      metric = user_friendly_variable_name,
      definition = variable_definition,
      years,
      updates = periodicity,
      units = format,
      source,
      url,
      citation
    ) %>% 
      filter(variable_name %in% vars) %>% 
      arrange(variable_name)
}
