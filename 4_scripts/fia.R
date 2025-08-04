# FIA
# 2025-08-04


# Description -------------------------------------------------------------


# Data from Forest Inventory Analysis. Querying from SQLite database. Would be
# worth figuring out how to avoid downloading whole thing though



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  dbplyr,
  RSQLite,
  vegan,
  readxl,
  tibble,
  tidyr,
  openxlsx2
)

conflicts_prefer(openxlsx2::read_xlsx)

# Invasive species list
invasive <- read_xlsx('1_raw/usfs/v9-5_2024-11_Natl_MasterInvasiveSpeciesList.xlsx')
get_str(invasive)

results <- list()



# sqldf -------------------------------------------------------------------


# pacman::p_load(sqldf)
# dat <- read.csv.sql(
#   '1_raw/usfs/CSV_FIADB_ENTIRE/ENTIRE_TREE.csv',
#   sql = 
#   'select
#     INVYR as year,
#     STATECD as state_fips,
#     COUNTYCD as county_fips,
#     SPCD as species,
#     DIA as dia
#   from file
#   where INVYR >= 2022'
# )
# get_str(dat)



# Chunking ----------------------------------------------------------------


pacman::p_load(
  arrow,
  dplyr,
  readr
)

# Filter function
chunk_filter <- function(df, pos) {
  df %>%
    select(INVYR, STATECD, COUNTYCD, SPCD, DIA) %>%
    filter(INVYR >= 2022)
}

output_file <- 'temp/chunked_output.csv'
if (file.exists(output_file)) file.remove(output_file)

read_csv_chunked(
 file = "1_raw/usfs/CSV_FIADB_ENTIRE/ENTIRE_TREE.csv",
 callback = SideEffectChunkCallback$new(function(df, pos) {
   filtered <- chunk_filter(df, pos)
   write_csv(filtered, output_file, append = file.exists(output_file))
 }),
 chunk_size = 100000
)

# Read it back
test <- read_csv('temp/chunked_output.csv')
get_size(test)
get_str(test)
# Noice



# SQLite ------------------------------------------------------------------


# # Connect to SQLite database
# con <- dbConnect(
#   RSQLite::SQLite(), 
#   "1_raw/usfs/SQLite_FIADB_ENTIRE/SQLite_FIADB_ENTIRE.db"
# )
# con
# 
# # Check col names
# (names <- dbListFields(con, 'tree'))
# 
# # Get only our variables, filter to 2000 and on
# out <- dbGetQuery(
#   con, 
#   'select 
#     INVYR as year, 
#     STATECD as state_fips, 
#     COUNTYCD as county_fips, 
#     SPCD as species, 
#     DIA as dia
#   from tree 
#   where INVYR >= 2000'
# )
# get_str(out)
# 
# # Fix fips
# dat <- out %>% 
#   mutate(
#     state_fips = sprintf("%02d", state_fips),
#     county_fips = sprintf("%03d", county_fips),
#     fips = paste0(state_fips, county_fips)
#   ) %>% 
#   select(year, fips, species, dia)
# get_str(dat)
# 
# # Get SD of distribution of diameter
# # Also diversity of species codes
# test <- dat %>% 
#   group_by(year, fips) %>% 
#   summarize(
#     # div = vegan::diversity(species),
#     sd_dia = sd(dia, na.rm = TRUE),
#     count = n()
#   )
# get_str(test)



# Wrangle -----------------------------------------------------------------


# Connect to SQLite database
con <- dbConnect(
  RSQLite::SQLite(),
  "1_raw/usfs/SQLite_FIADB_ENTIRE/SQLite_FIADB_ENTIRE.db"
)
tree <- tbl(con, 'tree')

# Filter data
tree <- tree %>%
  select(
    year = INVYR,
    state_fips = STATECD,
    county_fips = COUNTYCD,
    species = SPCD,
    dia = DIA
  ) %>% 
  filter(year >= 2000)
dat <- collect(tree)
get_str(dat)

# Fix fips
dat <- dat %>% 
  mutate(
    state_fips = sprintf("%02d", state_fips),
    county_fips = sprintf("%03d", county_fips),
    fips = paste0(state_fips, county_fips)
  ) %>% 
  select(year, fips, species, dia)
get_str(dat)



# Complexity --------------------------------------------------------------
## Diversity ---------------------------------------------------------------


# Check counts by county
dat %>% 
  group_by(year, fips) %>% 
  summarize(count = n()) %>% 
  pull(count) %>% 
  range()

# Get matrix of observations
matrix <- dat %>% 
  count(year, fips, species) %>% 
  pivot_wider(
    names_from = species,
    values_from = n,
    values_fill = 0
  )
matrix

# Get diversity
div <- matrix %>% 
  select(3:last_col()) %>% 
  diversity()
length(div)

# Put it back together
div <- matrix %>% 
  select(year, fips) %>% 
  mutate(treeDiversity = div)
get_str(div)

# Clean it up and save to results
results$div <- div %>% 
  pivot_longer(
    cols = treeDiversity,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(results$div)



## Size --------------------------------------------------------------------


get_str(dat)
sd_dia <- dat %>% 
  group_by(year, fips) %>% 
  summarize(sizeDiversity = sd(dia, na.rm = TRUE))
get_str(sd_dia)

# Pivot
results$sd_dia <- sd_dia %>% 
  pivot_longer(
    cols = sizeDiversity,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(results$sd_dia)



# Health ------------------------------------------------------------------


# Pull in invasive species codes to get proportion invasive
get_str(invasive)

# Get vector of invasive FIA codes
invasive_codes <- invasive %>% 
  select('FIA code') %>% 
  na.omit() %>% 
  pull()
invasive_codes  

# Get proportion invasive out of counts for each county
get_str(dat)
out <- dat %>% 
  group_by(year, fips) %>% 
  summarize(
    obs_count = n(),
    invasive_count = sum(species %in% invasive_codes),
    propInvasive = invasive_count / obs_count
  )
out
get_str(out)

# Arrange like others %>% 
results$invasive <- out %>% 
  select(year, fips, propInvasive) %>% 
  pivot_longer(
    cols = propInvasive,
    values_to = 'value',
    names_to = 'variable_name'
  )
get_str(results$invasive)



# Meta --------------------------------------------------------------------


results <- bind_rows(results)
meta_vars(results)

metas <- data.frame(
  dimension = 'environment',
  index = 'forests',
  indicator = c('forest_health', rep('forest complexity', 2)),
  metric = c(
    'Proportion of invasive species',
    'Tree size diversity',
    'Tree species diversity'
  ),
  axis_name = c(
    'Prop Invasive',
    'SD DBH',
    'Tree Diversity'
  ),
  definition = c(
    'Shannon index of diversity on tree species. Larger values are more diverse.',
    'Standard deviation of distribution of tree sizes as measured by DBH (diameter at breast height)',
    'Proportion of invasive species out of all observed species.'
  ),
  variable_name = meta_vars(results),
  units = c(
    'proportion',
    'standard deviations',
    'index'
  ),
  annotation = 'none',
  latest_year = meta_latest_year(results),
  year = meta_years(results),
  resolution = meta_resolution(results),
  scope = 'national',
  updates = 'annual',
  source = 'U.S. Department of Agriculture, Forest Service (2025). Forest Inventory Analysis.',
  url = 'https://research.fs.usda.gov/products/dataandtools/fia-datamart'
) %>% 
  meta_citation(date = '2025-08-04')
get_str(metas)



# Aggregate ---------------------------------------------------------------


# Check record counts
check_n_records(results, metas, 'Forest Inventory Analysis')

saveRDS(results, '5_objects/metrics/fia.RDS')
saveRDS(metas, '5_objects/metadata/fia_meta.RDS')

dbDisconnect(con)
clear_data(gc = TRUE)

