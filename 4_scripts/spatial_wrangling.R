#' Spatial Wrangling
#' 2024-10-22
#' 
#' Getting spatial features, counties, fips and states codes sorted out and 
#' saved to be used throughout project. Original script from USDA FAME warehouse



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  readr,
  purrr,
  janitor,
  skimr,
  tidyr,
  tidycensus
)



# Wrangle spatial data ----------------------------------------------------


# Vector of New England States
ne_states <- c('VT', 'NH', 'MA', 'ME', 'CT', 'RI') %>% 
  sort()

# Get county and state fips, state name, county name
county <- tidycensus::fips_codes %>% 
  filter(state %in% ne_states) %>% 
  unite(
    "fips", 
    c(state_code, county_code), 
    sep = "", 
    remove = FALSE
  ) %>% 
  rename(county_name = county) %>% 
  select(fips, county_name, state_name) %>% 
  `rownames<-`(NULL)

state <- tidycensus::fips_codes %>% 
  filter(state %in% ne_states) %>% 
  select(state_code, state_name) %>% 
  rename(fips = state_code) %>% 
  distinct() %>% 
  mutate(state_code = ne_states) %>% 
  `rownames<-`(NULL)

# Merge so we have county and state data in one data frame
county_state <- bind_rows(county, state) %>%
  add_row(
    fips = "00",
    county_name = NA,
    state_name = "US"
  )

# Import county spatial data frame
counties_2021 <- tigris::counties(
  state = ne_states, 
  progress_bar = TRUE,
  year = 2021
) %>% 
  clean_names() %>% 
  mutate(fips = paste0(statefp, countyfp)) %>% 
  select(fips, aland, awater, geometry)

# Import county spatial data frame
counties_2024 <- tigris::counties(
  state = ne_states, 
  progress_bar = TRUE,
  year = 2024
) %>% 
  clean_names() %>% 
  mutate(fips = paste0(statefp, countyfp)) %>% 
  select(fips, aland, awater, geometry)

states_2024 <- tigris::states(
  progress_bar = TRUE,
  year = 2024
) %>% 
  filter(STUSPS %in% ne_states) %>% 
  setNames(snakecase::to_snake_case(names(.))) %>% 
  select(
    fips = statefp,
    name,
    aland,
    awater,
    intptlat,
    intptlon,
    geometry
  )
states_2024


saveRDS(counties_2021, '2_clean/spatial/ne_counties_2021.RDS')
saveRDS(counties_2024, '2_clean/spatial/ne_counties_2024.RDS')
saveRDS(states_2024, '2_clean/spatial/ne_states_2024.RDS')

clear_data()