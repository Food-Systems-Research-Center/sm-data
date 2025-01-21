#' Spatial Wrangling
#' 2024-10-22
#' 
#' Getting spatial features, counties, fips and states codes sorted out and 
#' saved to be used throughout project. Original script from USDA FAME warehouse

#' Note that this script has been a slow drip of additions leading to some 
#' pretty gross code, and calling from tigris more than necessary. Update this
#' at some point.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  readr,
  purrr,
  janitor,
  skimr,
  tidyr,
  tidycensus,
  sf
)



# FIPS Keys ---------------------------------------------------------------


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
# This will be our fips key
county_state <- bind_rows(county, state) %>%
  add_row(
    fips = "00",
    county_name = NA,
    state_name = "US",
    state_code = 'US'
  )



# Spatial Data ------------------------------------------------------------


# Import county spatial data frame (before CT changes)
counties_2021 <- tigris::counties(
  state = ne_states, 
  progress_bar = TRUE,
  year = 2021
) %>% 
  clean_names() %>% 
  mutate(fips = paste0(statefp, countyfp)) %>% 
  select(fips, aland, awater, geometry)

# Import county spatial data frame for 2024 (after CT changes)
counties_2024 <- tigris::counties(
  state = ne_states, 
  progress_bar = TRUE,
  year = 2024
) %>% 
  clean_names() %>% 
  mutate(fips = paste0(statefp, countyfp)) %>% 
  select(fips, aland, awater, geometry)

# Get spatial data for all counties (using this for area later)
all_counties_2021 <- tigris::counties(
  state = NULL, 
  progress_bar = TRUE,
  year = 2021
) %>% 
  clean_names() %>% 
  mutate(fips = paste0(statefp, countyfp)) %>% 
  select(fips, aland, awater, geometry)

# Get spatial data for all counties (using this for area later)
all_counties_2024 <- tigris::counties(
  state = NULL, 
  progress_bar = TRUE,
  year = 2024
) %>% 
  clean_names() %>% 
  mutate(fips = paste0(statefp, countyfp)) %>% 
  select(fips, aland, awater, geometry)

# Also get spatial files for NE states
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

# And do it again for all states - not just NE
all_states_2024 <- tigris::states(
  progress_bar = TRUE,
  year = 2024
) %>% 
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

# Also just get fips codes for all states - keep these separate
all_state_codes <- fips_codes %>% 
  select(state, state_code, state_name) %>% 
  unique() %>% 
  filter(str_detect(
    state_name, 
    'Mariana|Guam|Rico|Islands|Samoa', 
    negate = TRUE
  )) %>% 
  mutate(full_state_code = paste0(state_code, '000'))

# One more vector of ALL relevant fips codes 
# This includes all NE counties, but also 51 states (not their counties)
all_fips <- c(
  all_state_codes$full_state_code,
  all_state_codes$state_code,
  county_state$fips
) %>% unique()



# Area DF -----------------------------------------------------------------


# creating DF with all county and state fips, and their area

## NE FIPS key
county_state
counties_2021
counties_2024

# Take 2024 counties first. Then just add remaining 2021 counties not included
most_areas <- reduce(list(all_counties_2024, all_states_2024), bind_rows)

# Pull just old 2021 counties from CT
old_ct <- all_counties_2021 %>% 
  filter(str_detect(fips, '^09.{2}[1-9]'))

# Combine old CT counties with rest of areas
areas <- bind_rows(most_areas, old_ct)

# Just get clean DF with fips and area, put areas in sq km units
area_df <- areas %>% 
  st_drop_geometry() %>% 
  select(fips, area_sqkm = aland, water_sqkm = awater) %>% 
  mutate(across(c(area_sqkm, water_sqkm), ~ round(.x / 1e6, 2)))
get_str(area_df)



# Save and Clear ----------------------------------------------------------


# Save everything as RDS and as gpkg where appropriate
saveRDS(county_state, '5_objects/fips_key.rds')
saveRDS(all_state_codes, '5_objects/state_key.rds')
saveRDS(all_fips, '5_objects/all_fips.rds')

saveRDS(counties_2021, '2_clean/spatial/ne_counties_2021.RDS')
st_write(counties_2021, '2_clean/spatial/ne_counties_2021.gpkg', append = FALSE)

saveRDS(counties_2024, '2_clean/spatial/ne_counties_2024.RDS')
st_write(counties_2024, '2_clean/spatial/ne_counties_2024.gpkg', append = FALSE)

saveRDS(states_2024, '2_clean/spatial/ne_states.RDS')
st_write(states_2024, '2_clean/spatial/ne_states.gpkg', append = FALSE)

saveRDS(all_states_2024, '2_clean/spatial/all_states.RDS')
st_write(all_states_2024, '2_clean/spatial/all_states.gpkg', append = FALSE)

saveRDS(all_counties_2021, '2_clean/spatial/all_counties_2021.RDS')
st_write(all_counties_2021, '2_clean/spatial/all_counties_2021.gpkg', append = FALSE)

saveRDS(all_counties_2024, '2_clean/spatial/all_counties_2024.RDS')
st_write(all_counties_2024, '2_clean/spatial/all_counties_2024.gpkg', append = FALSE)

# saveRDS(new_england, '2_clean/spatial/new_england.RDS')
# st_write(new_england, '2_clean/spatial/new_england.gpkg', append = FALSE)

saveRDS(area_df, '5_objects/areas.RDS')


# Clear
clear_data()
gc()
