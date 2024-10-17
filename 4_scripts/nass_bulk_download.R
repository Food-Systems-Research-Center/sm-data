#' NASS Bulk Download
#' 2024-09-24



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  readr,
  purrr,
  janitor,
  skimr,
  tidyr
)



# County Data -------------------------------------------------------------


# Vector of New England States
ne_states <- c('VT', 'NH', 'MA', 'ME', 'CT', 'RI') %>% 
  sort()

# Get county and state fips, state name, county name
county <- tidycensus::fips_codes %>% 
  filter(state %in% ne_states) %>% 
  unite("fips", 
        c(state_code, county_code), 
        sep = "", remove = FALSE) %>% 
  rename(county_name = county) %>% 
  select(fips, county_name, state_name)

state <- tidycensus::fips_codes %>% 
  filter(state %in% ne_states) %>% 
  select(state_code, state_name) %>% 
  rename(fips = state_code) %>% 
  distinct()

# Save these state codes
saveRDS(state, '5_objects/ne_state_codes.rds')

# Merge so we have county and state data in one data frame
county_state <- bind_rows(county, state)

# Manually add US as fips "00"
county_state <- county_state %>% 
  add_row(fips = "00", 
          county_name = NA, 
          state_name = "US")

# Save this as fips key for counties and states
saveRDS(county_state, '5_objects/fips_key.rds')

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

saveRDS(counties_2021, '2_clean/spatial/ne_counties_2021.RDS')
saveRDS(counties_2024, '2_clean/spatial/ne_counties_2024.RDS')

# Save the fips too
counties_2021$fips %>% 
  sort %>% 
  saveRDS('5_objects/fips_2021.rds')

counties_2024$fips %>% 
  sort %>% 
  saveRDS('5_objects/fips_2024.rds')

# And all fips
c(counties_2021$fips, counties_2024$fips) %>% 
  unique %>% 
  saveRDS('5_objects/fips_all.rds')



# CoA 2022 ----------------------------------------------------------------


# Pull whole CoA dataset
coa <- read_tsv('1_raw/qs.census2022.txt', show_col_types = FALSE) %>%
  janitor::clean_names()
get_size(coa)
get_str(coa)
# Already filtered to CoA, no survey here

# Filter to New England
ne_states <- c('VT', 'NH', 'ME', 'MA', 'CT', 'RI')
coa_ne <- coa %>%
  filter(state_alpha %in% ne_states)

get_size(coa_ne)
get_str(coa_ne)
# More manageable size.

# Get rid of full dataset now.
rm(coa)
gc()

# Cleaning a la Allison Bauman
get_str(coa_ne)
coa_ne <- coa_ne %>%
  mutate(
    county_code = case_when(county_code == "NULL" ~ NA, TRUE ~ county_code),
    county_name = case_when(county_name == "NULL" ~ NA, TRUE ~ county_name),
    value_codes = case_when(str_detect(value, "(D)") ~ "D", TRUE ~ NA),
    value = case_when(value == "(D)" ~ NA, TRUE ~ value),
    value = as.numeric(str_remove_all(value, ","))
  )

# Get state and county fips and convert US to fips "00"
# Then remove state data, keep only county
coa_ne <- coa_ne %>%
  filter(!is.na(county_code)) %>%
  mutate(fips = paste0(state_fips_code, county_code))
get_str(coa_ne)

# Save this as object for later
saveRDS(coa_ne, '5_objects/coa_ne.rds')



# Clear -------------------------------------------------------------------


clear_data()
