## code to prepare `metrics_example` dataset goes here
# Making a smaller example of metrics data for use in function examples
pacman::p_load(dplyr, stringr)
data(metrics)

# Get some vars
vars <- metrics %>% 
  filter(str_detect(
    variable_name, 
    regex('NAICS|^avg|^oty|High$|Low$', ignore_case = TRUE),
    negate = TRUE
  )) %>% 
  pull(variable_name) %>% 
  unique() %>% 
  sort()
head(vars, 25)

# Filter down to mini
metrics_example <- metrics %>% 
  filter(variable_name %in% c(vars[1:6]))

usethis::use_data(metrics_example, overwrite = TRUE)
