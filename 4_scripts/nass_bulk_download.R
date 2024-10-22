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
coa <- read_tsv('1_raw/qs.census2022.txt', show_col_types = FALSE) %>%
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



# Save and Clear ----------------------------------------------------------


saveRDS(coa_ne, '5_objects/coa_ne.rds')
clear_data()
