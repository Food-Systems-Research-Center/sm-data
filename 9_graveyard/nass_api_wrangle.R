
# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr
)

out <- readRDS('5_objects/api_outs/nass_census_counties_2007-2022.rds')



# Aggregate ---------------------------------------------------------------


get_str(out)
dat <- out %>% 
  flatten() %>% 
  bind_rows()
get_str(dat)

# Save into clean folder
saveRDS(dat, '1_raw/nass/nass_census_counties_2007-2022.rds')
