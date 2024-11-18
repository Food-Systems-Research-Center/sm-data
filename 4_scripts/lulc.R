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
  vegan,
  tibble,
  lubridate,
  reticulate
)

source('3_functions/read_all_rds.R')
source('3_functions/add_citation.R')
source('3_functions/check_n_records.R')

fips_key <- readRDS('5_objects/fips_key.rds')
ne_counties <- readRDS('2_clean/spatial/ne_counties_2024.RDS')

results <- list()
metas <- list()



# VT Biodiversity Project -------------------------------------------------


#' NOTE: Neither of these can be aggregated at county level. Hotspots are just
#' scattered polygons, and the atlas is at the town level, which cannot be 
#' aggregated to counties without knowing the species found in each one - it
#' only provides counts of totals. So we are just saving these to show in the 
#' map explorer, but can't add to data tables.

hotspots <- st_read(
  dsn = '1_raw/spatial/vt_bio_project/hotspots/',
  layer = 'VT_Biodiversity_Project_-_Biological_Hotspots'
)

atlas <- st_read(
  dsn = '1_raw/spatial/vt_bio_project/species_atlas/',
  layer = 'VT_Biodiversity_Project_-_Plant_and_Animal_Species_Atlas'
)

core <- read_stars(
  '1_raw/spatial/vt_bio_project/corehab/corehab/w001001.adf'
)



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

# NOTE: Copy code processing counties here. For now just loading
county_tables <- read_all_rds('1_raw/spatial/mrlc_lulc/counties/')
get_str(county_tables)

# Remove geometry and combine counties into one file with lulc diversity
# Also, use title as identifier. It is the only identifier of fips

# Get unique levels in case they are missing from one
all_levels <- map(county_tables, \(x) x$FUN) %>% 
  unlist() %>% 
  unique()

county_tables <- imap(county_tables, \(county, name) {
  county_fips <- str_split_i(name, '_', 2)
  out <- county %>% 
    select(
      lulc_code = FUN,
      cell_count = 3
    ) %>% 
    mutate(
      fips = county_fips
      # lulc_code = ifelse(is.na(lulc_code), 250, lulc_code)
    )
  levels(out$lulc_code) <- all_levels
  return(out)
})

get_str(county_tables)
# Will save this layer specifically below before processing diversity

# Descriptions of LULC codes. Removing perennial ice/snow (12) - none here
codes <- data.frame(
  lulc_code = all_levels,
  metric = c(
    'LULC, proportion, open water',
    # 'LULC, proportion, perennial ice/snow',
    'LULC, proportion, developed, open space',
    'LULC, proportion, developed, low intensity',
    'LULC, proportion, developed, medium intensity',
    'LULC, proportion, developed, high intensity',
    'LULC, proportion, barren land',
    'LULC, proportion, deciduous forest',
    'LULC, proportion, evergreen forest',
    'LULC, proportion, mixed forest',
    'LULC, proportion, shrub/scrub',
    'LULC, proportion, grassland/herbaceous',
    'LULC, proportion, pasture/hay',
    'LULC, proportion, cultivated crops',
    'LULC, proportion, woody wetlands',
    'LULC, proportion, emergent herbaceous wetlands',
    'LULC, proportion, no data'
  ),
  definition = c(
    'Areas of open water, generally with less than 25% cover of vegetation or soil',
    # 'Areas characterized by a perennial cover of ice and/or snow',
    'Areas with a mixture of some constructed materials, but mostly vegetation in the form of lawn grasses. Impervious surfaces account for less than 20% of total cover. These areas most commonly include large-lot single-family housing units, parks, golf courses, and vegetation planted in developed settings for recreation, erosion control, or aeshetic purposes',
    'Areas with a mixture of constructed materials and vegetation. Impervious surfaces account for 20% to 49% percent of total cover. Thesea reas most commonly include single-family housing units.',
    'Areas with a mixture of constructed materials and vegetation. Impervious surfaces account for 50 % to 79% of the total cover. These areas most commonly include single - family housing units.',
    'Highly developed areas where people reside or work in high numbers. Examples include apartment complexes, row houses and commercial/industrial. Impervious surfaces account for 80% to 100% of the total cover.',
    'Areas of bedrock, desert pavement, scarps, talus, slides, volcanic material, glacial debris, sand dunes, strip mines, gravel pits and other accumulations of earthen material. Generally, vegetation accounts for less than 15% of total cover.',
    'Areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. More than 75% of the tree species shed foliage simultaneously in response to seasonal change.',
    'Areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. More than 75% of the tree species maintain their leaves all year. Canopy is never without green foliage.',
    'Areas dominated by trees generally greater than 5 meters tall, and greater than 20% of total vegetation cover. Neither deciduous nor evergreen species are greater than 75% of total tree cover.',
    'Areas dominated by shrubs; less than 5 meters tall with shrub canopy typically greater than 20% of total vegetation. This class includes true shrubs, young trees in an early successional stage or trees stunted from environmental conditions.',
    'Areas dominated by graminoid or herbaceous vegetation, generally greater than 80% of total vegetation. These areas are not subject to intensive management such as tilling but can be utilized for grazing.',
    'Areas of grasses, legumes, or grass-legume mixtures planted for livestock grazing or the production of seed or hay crops, typically on a perennial cycle. Pasture/hay vegetation accounts for greater than 20% of total vegetation.',
    'Areas used to produce annual crops, such as corn, soybeans, vegetables, tobacco, and cotton, and perennial woody crops such as orchards and vineyards. Crop vegetation accounts for greater than 20% of total vegetation. This class also includes all land being actively tilled.',
    'Areas where forest or shrubland vegetation accounts for greater than 20% of vegetative cover and the soil or substrate is periodically saturated with or covered with water.',
    'Areas where perennial herbaceous vegetation accounts for greater than 80% of vegetative cover and the soil or substrate is periodically saturated with or covered with water.',
    'No data.'
  )
)
codes

# Turn into proportions
prop <- map(county_tables, \(x) {
  x %>% 
    mutate(prop = round(cell_count / sum(x$cell_count), 3))
}) %>% 
  bind_rows() %>% 
  left_join(codes) %>% 
  select(
    fips,
    value = prop,
    variable_name = metric
  ) %>% 
  mutate(year = '2023')
get_str(prop)
prop

# Save this to results
results$lulc_prop <- prop



## Props metadata ----------------------------------------------------------


metas$lulc_prop <- codes %>% 
  mutate(
    variable_name = metric %>% 
      str_remove_all(',|ortion|intensity|aceous|eloped|cultivated|emergent') %>% 
      snakecase::to_lower_camel_case(),
    axis_name = str_remove_all(definition, 'proportion, '),
    dimension = "environment",
    index = 'biodiversity',
    indicator = 'land use diversity',
    units = 'proportion',
    scope = 'national',
    resolution = '30m',
    year = '2023',
    latest_year = '2023',
    updates = "annual",
    source = paste(
      'U.S. Geological Survey, Multi-Resolution Land Characteristics Consortium (2024).',
      'Sioux Falls, SD'
    ),
    url = 'https://www.mrlc.gov/data/type/land-cover'
  ) %>% 
  add_citation(access_date = '2024-11-15')

get_str(metas$lulc_prop)



## Diversity ---------------------------------------------------------------


# Shannon diversity of LULC by group
get_str(county_tables)
div <- map_dbl(county_tables, \(x) {
  df <- x %>% 
    mutate(
      lulc_code = case_when(
        lulc_code %in% c(11, 12) ~ 'water',
        str_detect(lulc_code, '^2') ~ 'developed',
        lulc_code == 31 ~ 'barren',
        str_detect(lulc_code, '^4') ~ 'forest',
        lulc_code == 52 ~ 'shrub',
        lulc_code == 71 ~ 'grass',
        str_detect(lulc_code, '^8') ~ 'forest',
        str_detect(lulc_code, '^9') ~ 'wetlands'
      )
    )
  out <- diversity(df$cell_count, index = 'shannon')
  return(out)
}) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('fips', 'value')) %>% 
  mutate(
    fips = str_split_i(fips, '_', 2),
    year = '2023',
    variable_name = 'lulcDiversity'
  )
div

# save to results list
results$lulc_div <- div


## Save a map layer for Quarto also
ne_counties %>% 
  right_join(div, by = 'fips') %>% 
  left_join(fips_key, by = 'fips') %>% 
  select(fips, county_name, lulc_div = value, geometry) %>% 
  saveRDS('2_clean/spatial/map_layers/lulc_div.rds')



### Metadata --------------------------------------------------------------


metas$lulc_div <- data.frame(
  variable_name = 'lulcDiversity',
  definition = paste(
    'Shannon diversity of LULC codes from MRLC Land Use Land Cover 30m layer by county.',
    'LULC Codes grouped by category: developed, barren, forest, shrubland, herbaceous, cultivated, and wetlands.',
    'Larger numbers represent greater diversity of LULC.'
  ),
  axis_name = 'LULC Diversity',
  dimension = "environment",
  index = 'biodiversity',
  indicator = 'land use diversity',
  units = 'index',
  scope = 'national',
  resolution = '30m',
  year = '2023',
  latest_year = '2023',
  updates = "annual",
  source = paste(
    'U.S. Geological Survey, Multi-Resolution Land Characteristics Consortium (2024).',
    'Sioux Falls, SD'
  ),
  url = 'https://www.mrlc.gov/data/type/land-cover'
) %>% 
  add_citation(access_date = '2024-11-15')

get_str(metas$lulc_div)



# Imperiled Species -------------------------------------------------------


# NOTE: This is not very useful - almost nothing going on in new england.

# Richness of imperiled species
# https://natureserve.maps.arcgis.com/home/item.html?id=5621d4789e174cc2b0695bfecd6dc6a8

# rip <- read_stars('1_raw/spatial/nature_serve/richness_imperiled_species.tif')
# rip



# TreeMap 2016 ------------------------------------------------------------


# https://www.fs.usda.gov/rds/archive/catalog/RDS-2021-0074
treemap <- read_stars('1_raw/spatial/usfs_treemap/TreeMap2016_CARBON_L.tif')

# Crop
ne_counties_prj <- st_transform(ne_counties, st_crs(treemap))
treemap_crop <- st_crop(treemap, ne_counties_prj)

# Saving this raster to use straight up in Quarto
# write_stars(treemap_crop, '2_clean/spatial/map_layers/treemap_biomass.tif')
# NOTE: This is too big to get into Quarto conveniently...

# Saving the counties file to a shapefile to use in python
st_crs(ne_counties_prj)
st_write(
  ne_counties,
  '5_objects/spatial/counties_shapefile.shp',
  append = FALSE
)


## Pull it back in after aggregating in python
agg <- st_read('5_objects/spatial/aggregated_biomass', 'biomass_by_counties')

# Have to put the crs back in place
st_crs(agg) <- st_crs(treemap)

# Save a copy to use straight in map
agg <- agg %>% 
  left_join(fips_key, by = 'fips') %>% 
  select(fips, mean_biomass = mean, county_name, geometry) %>% 
  mutate(mean_biomass = round(mean_biomass, 3))

saveRDS(agg, '2_clean/spatial/map_layers/mean_biomass.rds')

# Now wrangle a copy like normal dataset
agg <- agg %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  mutate(
    variable_name = 'meanAboveGrndForBiomass',
    year = '2016'
  ) %>% 
  rename(value = mean_biomass)
get_str(agg)

# Save to results
results$mean_biomass <- agg



## Metadata ----------------------------------------------------------------


get_str(agg)

metas$bio_means <- data.frame(
  variable_name = unique(agg$variable_name),
  definition = 'Above ground forest biomass, live, tons per acre',
  axis_name = 'Live Biomass (tons / acre)',
  dimension = "environment",
  index = 'biodiversity',
  indicator = 'above ground biomass',
  units = 'tons / acre',
  scope = 'national',
  resolution = '30m',
  year = '2016',
  latest_year = '2016',
  updates = "unknown",
  source = 'Riley, Karin L.; Grenfell, Isaac C.; Shaw, John D.; Finney, Mark A. 2022. TreeMap 2016 dataset generates CONUS-wide maps of forest characteristics including live basal area, aboveground carbon, and number of trees per acre',
  url = 'https://doi.org/10.1093/jofore/fvac022',
  citation = 'Riley, Karin L.; Grenfell, Isaac C.; Shaw, John D.; Finney, Mark A. 2022. TreeMap 2016 dataset generates CONUS-wide maps of forest characteristics including live basal area, aboveground carbon, and number of trees per acre. Journal of Forestry. 2022: 607-632.'
)

get_str(metas$bio_means)



# Save and Clear ----------------------------------------------------------


result <- results %>% 
  map(\(x) mutate(x, value = as.character(value))) %>% 
  bind_rows()
get_str(result)

meta <- metas %>% 
  bind_rows()
get_str(meta)

# Check record counts
check_n_records(result, meta, 'lulc')

saveRDS(result, '5_objects/metrics/lulc.RDS')
saveRDS(meta, '5_objects/metadata/lulc_meta.RDS')

# Also save spatial files as is
saveRDS(div, '5_objects/spatial/county_lulc_tables.rds')
saveRDS(hotspots, '2_clean/spatial/map_layers/hotspots.rds')
saveRDS(atlas, '2_clean/spatial/map_layers/atlas.rds')
# write_stars(core, '5_objects/spatial/rasters/core_habitat.tif')

clear_data()
