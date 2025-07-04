# Sandbox


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr
)

# Clean, aggregated data
dat <- readRDS('2_clean/metrics.rds')
meta <- readRDS('2_clean/metadata.rds')



# Check employee numbers --------------------------------------------------


get_str(meta)
meta$variable_name



# -------------------------------------------------------------------------


get_str(metrics)

metrics %>% 
  filter(
    str_detect(
      variable_name, 
      regex('naics|^lq|^avgEmpLevel', ignore_case = TRUE), 
      negate = TRUE
    )
  ) %>% 
  pull(variable_name) %>% 
  unique() %>% 
  sort()

dat <- metrics %>% 
  filter(str_detect(variable_name, '^oty'))
get_str(dat)



# test saving space -------------------------------------------------------


pacman::p_load(
  dplyr,
  readr
)

metrics <- read_csv('6_outputs/metrics.csv')
get_size(metrics)

dat <- metrics %>% 
  mutate(variable_name = dense_rank(variable_name))
get_str(dat)
get_size(dat)
# not much better


sm_data <- readRDS('6_outputs/sm_data.rds')
get_size(sm_data)
