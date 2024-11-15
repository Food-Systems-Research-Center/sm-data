# LULC
# 2024-09-13

# Wrangling 2022 VT Open Data Base Land Cover
# https://geodata.vermont.gov/pages/ba998c98930f474c97aaf3bd44f1f694


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  sf,
  stars,
  dplyr,
  mapview,
  terra,
  stringr,
  purrr,
  furrr,
  tictoc,
  vegan
)


fips_key <- readRDS('5_objects/fips_key.rds')
ne_counties <- readRDS('2_clean/spatial/ne_counties_2024.RDS')



# VT Biodiversity Project -------------------------------------------------


hotspots <- st_read(
  dsn = '1_raw/spatial/vt_bio_project/hotspots/',
  layer = 'VT_Biodiversity_Project_-_Biological_Hotspots'
)
hotspots

atlas <- st_read(
  dsn = '1_raw/spatial/vt_bio_project/species_atlas/',
  layer = 'VT_Biodiversity_Project_-_Plant_and_Animal_Species_Atlas'
)
atlas
# Just save these as sf objects to put in map I guess?



# LULC --------------------------------------------------------------------


# Trying to get land use diversity
lulc <- read_stars('1_raw/spatial/mrlc_lulc/Annual_NLCD_LndCov_2023_CU_C1V0.tif')

# Crop by New England, but first reproject our counties
ne_counties_prj <- st_transform(ne_counties, st_crs(lulc))
crop <- st_crop(lulc, ne_counties_prj)

# Get table of values in each county
lulc_by_county = function(x) {
  table(x, useNA = 'always')
}

saveRDS(crop, '5_objects/lulc_crop.rds')

# # Just once
# test <- aggregate(
#   crop, 
#   ne_counties_prj, 
#   FUN = lulc_by_county
# )
# test$Annual_NLCD_LndCov_2023_CU_C1V0.tif
# test %>% 
#   as.data.frame() %>% 
#   select(-geometry)


# Test map
# out <- ne_counties_prj[1:2, ] %>% 
#   split(seq(nrow(ne_counties_prj[1:2, ]))) %>% 
#   map(\(row) {
#     aggregate(crop, row, FUN = lulc_by_county)
#   })
# out
# get_str(out)
# This might be the way - easy to move into parallel here

# check it
out[[1]] %>% 
  as.data.frame()
test <- out %>% 
  imap(~ {
    .x %>% 
      as.data.frame() %>%
      pull(3) %>% 
      diversity()
  })
test



# Prep for parallel
counties <- ne_counties_prj %>% 
  split(seq(nrow(ne_counties_prj)))
county_fips <- ne_counties_prj$fips
config <- furrr_options(seed = TRUE)

# Run parallel
format(Sys.time(), '%H:%M:%S')
tic()
plan(multisession, workers = 6)
out <- future_map(counties, \(county) {
    aggregate(crop, county, FUN = lulc_by_county)
  }, .options = config, .progress = TRUE) %>% 
  setNames(c(county_fips))
plan(sequential)
toc()



# Save and Clear ----------------------------------------------------------


saveRDS(hotspots, '5_objects/spatial/hotspots.rds')
saveRDS(atlas, '5_objects/spatial/atlas.rds')
