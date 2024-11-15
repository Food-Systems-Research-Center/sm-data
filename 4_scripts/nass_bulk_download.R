#' NASS Bulk Download
#' 2024-10-22 update

#' Filtering down bulk download of USDA NASS Census of Agriculture 2022 and
#' prepping it to extract relevant variables in next script.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  readr,
  janitor,
  tidyr
)

state_key <- readRDS('5_objects/state_key.rds')
fips_key <- readRDS('5_objects/fips_key.rds')



# CoA 2022 ----------------------------------------------------------------


# Pull whole CoA dataset
coa <- read_tsv('1_raw/nass/qs.census2022.txt', show_col_types = FALSE) %>%
  janitor::clean_names()

# Filter to New England
coa_ne <- coa %>%
  filter(state_alpha %in% unique(fips_key$state_code))

# rm(coa)
# gc()

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
# Note that we are keeping state data here
coa_ne <- coa_ne %>%
  # mutate(fips = paste0(state_fips_code, county_code))
  mutate(fips = case_when(
    is.na(county_code) ~ state_fips_code,
    .default = paste0(state_fips_code, county_code)
  ))
# get_str(coa_ne)



# NAICS codes -------------------------------------------------------------


# Pulling relevant codes as identified by Allison Bauman 
food_codes <- c(
  '111',
  '112',
  '114',
  '115',
  '311',
  '484',
  '493',
  '445',
  '722',
  '3111',
  '3112',
  '3113',
  '3114',
  '3115',
  '3116',
  '3117',
  '3118',
  '3119',
  '3121',
  '3122',
  '4244',
  '4245',
  '4248',
  '4451',
  '4452',
  '4453',
  '7223',
  '7224',
  '7225',
  '32532',
  '33311',
  '42491',
  '44511',
  '44512',
  '311811',
  '72232',
  '72233',
  '722511'
)

# Load codes and titles from BLS
naics_codes <- read_csv('1_raw/bls/2022_titles_descriptions.csv')
get_str(naics_codes)

# Filter the full set of NAICS codes down to the food codes
naics_key <- naics_codes %>% 
  setNames(c('naics', 'short_title', 'full_title', 'description')) %>% 
  filter(naics %in% food_codes)
get_str(naics_key)



# Save and Clear ----------------------------------------------------------


saveRDS(coa_ne, '1_raw/nass/coa_ne.rds')
saveRDS(naics_key, '5_objects/naics_key.rds')

clear_data()
