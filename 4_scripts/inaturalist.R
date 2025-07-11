# iNaturalist
# 2025-07-11


# Description -------------------------------------------------------------


# Using iNaturalist observations to get species counts by taxa at county level
# across Northeast counties



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  readr,
  sf,
  mapview,
  vegan,
  tidyr
)



# Test --------------------------------------------------------------------


# Using just 2024 data across Northeast
dat <- read_tsv('1_raw/inaturalist/0001192-250711103210423.csv')
get_str(dat)

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
# This will be our dataset

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
    values_from = c(n_spp, n_obs, div),
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
        .default = NA
      )
    ) %>% 
      snakecase::to_sentence_case(),
    definition = case_when(
      str_detect(variable_name, 'diversity') ~ 'Shannon diversity of species within kingdom',
      str_detect(variable_name, 'species') ~ 'Number of species within kingdom',
      str_detect(variable_name, 'observations') ~ 'Number of observations within kingdom',
      .default = NA
    ),
    units = case_when(
      str_detect(variable_name, 'diversity') ~ 'index',
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
