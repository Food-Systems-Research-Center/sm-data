# iNaturalist
# 2025-08-05 update


# Description -------------------------------------------------------------


# Using iNaturalist observations to get species counts by taxa at county level
# across Northeast counties. Bulk download through GBIF for just northeast 
# states. This zip is already ~20GB, so doing this for CONUS would be 
# challenging.

# Note that a better way to process might be reading chunked csv from the
# download link itself. However, this will probably take about an hour. To
# download and then unzip.

# Download link: 
# https://api.gbif.org/v1/occurrence/download/request/0064098-250717081556266.zip



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  readr,
  sf,
  mapview,
  vegan,
  tidyr,
  arrow
)



# Load Data ---------------------------------------------------------------
## Load 2010-2023 ----------------------------------------------------------


# Processing in chunks, saving as parquet files
output_dir <- "1_raw/inaturalist/parquet_chunks"
if (!dir.exists(output_dir)) dir.create(output_dir)

chunk_filter <- function(df) {
  df[df$kingdom %in% c("Animalia", "Fungi", "Plantae") & df$year >= 2010,
     c("kingdom", "species", "year", "decimalLatitude", "decimalLongitude")]
}

# Counter to name each chunk file uniquely
chunk_id <- 0

callback <- SideEffectChunkCallback$new(function(df, pos) {
  filtered <- chunk_filter(df)
  # Update chunk ID
  chunk_id <<- chunk_id + 1
  chunk_path <- file.path(output_dir, paste0("chunk_", chunk_id, ".parquet"))
  # Write filtered chunk as Parquet file, remove files and clear memory
  arrow::write_parquet(filtered, sink = chunk_path)
  rm(filtered, df)
  gc()
})

read_tsv_chunked(
  # file = '1_raw/inaturalist/0001192-250711103210423.csv', # Tester
  file = "1_raw/inaturalist/0064098-250717081556266/0064098-250717081556266.csv", # Real thing
  callback = callback,
  chunk_size = 100000 
)
# Issue with the last 5% - R caps out at 100 million. Not sure why this is
# happening though

# Read all parquet files together
dataset <- open_dataset(output_dir, format = "parquet")
dat <- collect(dataset)
get_str(dat)
sort(unique(dat$year))



## Load 2010-2023 ----------------------------------------------------------


# # Loading large file for northeast states 2010-2023. We also add the 2024 
# # data to this one below.
# 
# # Check headers from smaller file
# headers <- read_tsv(
#   '1_raw/inaturalist/0001192-250711103210423.csv',
#   n_max = 0
# ) %>% 
#   names()
# headers
# 
# 
# # Filter function for each chunk
# chunk_filter <- function(df, pos) {
#   df %>%
#     select(kingdom, species, year, decimalLatitude, decimalLongitude) %>%
#     filter(
#       kingdom %in% c('Animalia', 'Fungi', 'Plantae'),
#       year >= 2010
#     )
#   
#   
# }
# 
# output_file <- '1_raw/inaturalist/preprocessed.csv'
# if (file.exists(output_file)) file.remove(output_file)
# 
# # Read in chunks and save back to smaller csv
# # Automatically unzips
# read_tsv_chunked(
#   file = "1_raw/inaturalist/0064098-250717081556266/0064098-250717081556266.csv",
#   # file = "https://api.gbif.org/v1/occurrence/download/request/0064098-250717081556266.zip",
#   callback = SideEffectChunkCallback$new(function(df, pos) {
#     filtered <- chunk_filter(df, pos)
#     write_csv(filtered, output_file, append = file.exists(output_file))
#     write.table(filtered, output_file,
#                 append = file.exists(output_file),
#                 sep = ",", col.names = !file.exists(output_file),
#                 row.names = FALSE, quote = TRUE)
#     rm(filtered)
#     gc()
#   }),
#   chunk_size = 250000
# )
# 
# 
# # Read it back to test
# # test <- read_csv('1_raw/inaturalist/preprocessed.csv')
# test <- read_csv(output_file)
# get_size(test)
# get_str(test)



## Load 2024 ---------------------------------------------------------------


# Using just 2024 data across Northeast as a test run
dat <- read_tsv('1_raw/inaturalist/0001192-250711103210423.csv')
get_str(dat)
unique(dat$year)

# Check unique species key vs species
length(unique(dat$speciesKey))
length(unique(dat$species))
length(unique(dat$scientificName))
# Let's use species, don't worry about subspecies in scientific name

# Kingdoms
get_table(dat$kingdom)

# Filter down to just plants, animals, fungi
# Then take relevant cols only
dat <- filter(dat, kingdom %in% c('Animalia', 'Fungi', 'Plantae')) %>% 
  select(kingdom, species, year, decimalLatitude, decimalLongitude)  
get_str(dat)



# COMBINE HERE ------------------------------------------------------------


# 2024:
get_str(dat)

# Read all parquet files together from 2010 to 2023
dataset <- open_dataset(output_dir, format = "parquet")
more_dat <- collect(dataset)
get_str(more_dat)

# Combine them
combine <- bind_rows(dat, more_dat) %>% 
  drop_na(decimalLongitude, decimalLatitude)
get_str(combine)



# New Spatial -------------------------------------------------------------


# TODO: need to process each parquet separately. All the way through
# group_by. Then put them together

# Convert to sf object
dat <- st_as_sf(combine, coords = c('decimalLongitude', 'decimalLatitude'))
class(dat)

# Set CRS for points to WGS
st_crs(dat) <- 4326

# Bring in counties, check crs
neast_counties_2024
st_crs(neast_counties_2024)

# Get counties into same crs as points
county_prj <- st_transform(neast_counties_2024, st_crs(dat))

# Map points over counties
# mapview(dat, col.regions = 'red') + mapview(county_prj)

# Get counties for each point
## TODO
dat <- st_join(dat, county_prj)
get_str(dat)

# Remove other columns, drop geometry
dat <- dat %>% 
  select(kingdom, species, year, fips) %>% 
  st_drop_geometry()
get_str(dat)



# Spatial -----------------------------------------------------------------


# Convert to sf object
dat <- st_as_sf(dat, coords = c('decimalLongitude', 'decimalLatitude'))

# Set CRS for points to WGS
st_crs(dat) <- 4326

# Bring in counties, check crs
neast_counties_2024
st_crs(neast_counties_2024)

# Get counties into same crs as points
county_prj <- st_transform(neast_counties_2024, st_crs(dat))

# Map points over counties
# mapview(dat, col.regions = 'red') + mapview(county_prj)

# Get counties for each point
dat <- st_join(dat, county_prj)
get_str(dat)

# Remove other columns, drop geometry
dat <- dat %>% 
  select(kingdom, species, year, fips) %>% 
  st_drop_geometry()
get_str(dat)



# Rarefy ------------------------------------------------------------------


# Get abundance table, filtering for >100 observations
get_str(dat)
dat_n100 <- dat %>%
  group_by(fips, year, kingdom) %>%
  filter(n() >= 100) %>%
  ungroup()
get_str(dat_n100)

abund_table <- dat_n100 %>%
  group_by(fips, year, kingdom, species) %>%
  summarize(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = species,
    values_from = n,
    values_fill = 0
  )
abund_table

abund_only <- abund_table %>%
  select(-fips, -year, -kingdom)
abund_only

min_n <- min(rowSums(abund_only))

# Rarefy
rare_rich <- rarefy(abund_only, sample = min_n)

# Combine with grouping info
abund_table$rare_rich <- rare_rich
rare_rich <- abund_table %>% 
  select(fips, year, kingdom, rare_rich)
get_str(rare_rich)

# Make name 3 characters to match others
rare_rich <- rename(rare_rich, rar = rare_rich)
get_str(rare_rich)



# Continue ----------------------------------------------------------------


# Get counts of observations, species, and diversity by county
dat <- dat %>%
  group_by(fips, year, kingdom) %>%
  summarize(
    n_spp = n_distinct(species),
    n_obs = n(),
    div = diversity(table(species)),
    .groups = "drop"
  )
get_str(dat)

# Add rarefied richness to check bias
dat <- left_join(dat, rare_rich)
get_str(dat)

# # Map diversity by county to check
# check <- out %>% 
#   filter(kingdom == 'Animalia') %>% 
#   right_join(neast_counties_2024) %>% 
#   st_as_sf()
# get_str(check)
# mapview(check, zcol = 'div')

# Get into format for metrics
get_str(dat)
dat <- dat %>%
  pivot_wider(
    id_cols = c(fips, year),
    names_from = kingdom,
    values_from = c(n_spp, n_obs, div, rar),
    names_glue = "{kingdom}_{.value}"
  )
get_str(dat)

# Rename cols
dat <- dat %>% 
  setNames(c(names(dat) %>% 
               str_replace('Animalia', 'animal') %>% 
               str_replace('Fungi', 'fungus') %>% 
               str_replace('Plantae', 'plant') %>% 
               str_remove('_n') %>% 
               snakecase::to_lower_camel_case()
             ))
get_str(dat)  
  
# Now pivot longer to put in format for metrics
dat <- dat %>% 
  pivot_longer(
    cols = !c(fips, year),
    names_to = 'variable_name',
    values_to = 'value'
  )
get_str(dat)    



# Metadata ----------------------------------------------------------------


(vars <- meta_vars(dat))

meta <- data.frame(
  variable_name = vars
) %>% 
  mutate(
    dimension = 'environment',
    index = 'species and habitat',
    indicator = 'biodiversity', 
    metric = paste(
      str_sub(variable_name, end = -4),
      case_when(
        str_detect(variable_name, 'Spp$') ~ 'species',
        str_detect(variable_name, 'Obs$') ~ 'observations',
        str_detect(variable_name, 'Div$') ~ 'diversity',
        str_detect(variable_name, 'Rar$') ~ 'rarefied richness',
        .default = NA
      )
    ) %>% 
      snakecase::to_sentence_case(),
    definition = case_when(
      str_detect(metric, 'diversity') ~ 'Shannon diversity of species within kingdom',
      str_detect(metric, 'species') ~ 'Number of species within kingdom',
      str_detect(metric, 'observations') ~ 'Number of observations within kingdom',
      str_detect(metric, 'rarefied richnes') ~ 'Rarefied richness of observations within kingdom, truncated at counties wtih > 100 total observations',
      .default = NA
    ),
    units = case_when(
      str_detect(metric, 'diversity|rarefied') ~ 'index',
      .default = 'count'
    ),
    scope = 'national',
    resolution = meta_resolution(dat),
    year = meta_years(dat),
    latest_year = meta_latest_year(dat),
    updates = "continuous",
    source = 'iNaturalist',
    url = 'https://map.feedingamerica.org/'
  ) %>% 
    meta_citation(date = '2025-07-11')
get_str(meta)



# Save --------------------------------------------------------------------


## Check to make sure we have the same number of metrics and metas
check_n_records(dat, meta, 'iNaturalist')

# Save them
saveRDS(dat, '5_objects/metrics/inaturalist.RDS')
saveRDS(meta, '5_objects/metadata/inaturalist.RDS')

clear_data(gc = TRUE)
