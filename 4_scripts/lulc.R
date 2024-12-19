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
  reticulate,
  tidyr
)

source('3_functions/pipeline_utilities.R')
source('3_functions/metadata_utilities.R')

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

# Save a copy to include straight into docs. Also as R object, see if faster
saveRDS(crop, '2_clean/spatial/map_layers/mrlc_lulc_ne.rds')


## Get cell counts of each category for each county
county_path <- '2_clean/spatial/ne_counties_2024.gpkg'
state_path <- '2_clean/spatial/ne_states.gpkg'
raster_path <- '1_raw/spatial/mrlc_lulc/Annual_NLCD_LndCov_2023_CU_C1V0.tif'

# Load python function, get cell counts for county and state
reticulate::source_python('3_functions/spatial/cat_zonal_stats.py')
out <- map(list(county_path, state_path), ~ cat_zonal_stats(raster_path, .x))
get_str(out)

# Get unique levels. Use later to make sure we are not missing any
all_levels <- names(out[[1]])[-which(names(out[[1]]) == 'fips')]

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
    'LULC, proportion, emergent herbaceous wetlands'
    # 'LULC, proportion, no data'
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
    'Areas where perennial herbaceous vegetation accounts for greater than 80% of vegetative cover and the soil or substrate is periodically saturated with or covered with water.'
    # 'No data.'
  )
)

# Make variable names based on metrics
removals <- c(',', 'ortion', 'eloped', 'ensity', '/scrub', 'aceous', 'emergent', '/hay', 'ium', 'iduous', 'ivated')
codes$variable_name <- reduce(removals, ~ str_remove_all(.x, .y), .init = codes$metric) %>% 
  str_replace_all('/', ' ') %>% 
  snakecase::to_lower_camel_case()
get_str(codes)

# rowSums to get proportions of each LULC type. Each will be a different metric
# Also turn NAs into 0s - there is only one, no cropland in that cell
all_lulc <- map(out, ~ {
  .x %>% 
    mutate(
      across(everything(), ~ ifelse(is.na(.x), 0, .x)),
      total_cells = rowSums(select(., -fips), na.rm = TRUE),
      across(c('11':'95'), ~ round(.x / total_cells, 3))
    ) %>% 
    select(-total_cells) %>% 
    
    # Rename columns based on codes df
    setNames(c(
      codes$variable_name[match(names(.)[-length(names(.))], codes$lulc_code)],
      names(.)[length(names(.))]
    ))
})
get_str(all_lulc)
# Saving this here because we will use it for LULC Diversity

# Continue getting our metrics in long format
all_lulc_metrics <- map(all_lulc, ~ {
  .x %>% 
    pivot_longer(
      cols = !fips,
      names_to = 'variable_name',
      values_to = 'value'
    )
}) %>% 
  bind_rows() %>% 
  mutate(year = '2023')
get_str(all_lulc_metrics)

# Save this to results
results$lulc <- all_lulc_metrics



## Metadata ----------------------------------------------------------


(vars <- get_vars(results$lulc))

metas$lulc_prop <- codes %>% 
  mutate(
    definition = paste0(
      'LULC ',
      lulc_code,
      ': ',
      definition
    ),
    axis_name = variable_name,
    dimension = "environment",
    index = 'biodiversity',
    indicator = 'land use diversity',
    units = 'proportion',
    scope = 'national',
    resolution = 'county, state',
    year = '2023',
    latest_year = '2023',
    updates = "annual",
    source = paste(
      'U.S. Geological Survey, Multi-Resolution Land Characteristics Consortium (2024).',
      'Sioux Falls, SD'
    ),
    url = 'https://www.mrlc.gov/data/type/land-cover'
  ) %>% 
  add_citation(access_date = '2024-11-15') %>% 
  select(-lulc_code)

get_str(metas$lulc_prop)



# LULC Diversity ---------------------------------------------------------------


# Shannon diversity of LULC by group
get_str(all_lulc)
div <- map(all_lulc, ~ {
  .x %>% 
    column_to_rownames('fips') %>% 
    select(-c(matches('Dev|Barren'))) %>% 
    diversity() %>%
    round(3) %>% 
    format(nsmall = 3) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c('fips', 'lulcDiversity')) %>% 
    pivot_longer(
      cols = !fips,
      names_to = 'variable_name',
      values_to = 'value'
    ) %>% 
    mutate(year = '2023')
}) %>% 
  bind_rows()
div

# Save
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
  metric = 'Land Use Diversity',
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
  resolution = 'county, state',
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



# TreeMap 2016 ------------------------------------------------------------


# This is just live carbon layer. Doing more below
# https://www.fs.usda.gov/rds/archive/catalog/RDS-2021-0074
treemap <- read_stars('1_raw/spatial/usfs_treemap/TreeMap2016_CARBON_L.tif')

# Crop
ne_counties_prj <- st_transform(ne_counties, st_crs(treemap))
treemap_crop <- st_crop(treemap, ne_counties_prj)

# Saving this raster to use straight up in Quarto
saveRDS(treemap_crop, '2_clean/spatial/map_layers/treemap_biomass.rds')



# Redo TreeMap ------------------------------------------------------------


# Run python script to aggregate TreeMap2016 data by counties
county_path = '2_clean/spatial/ne_counties_2024.gpkg'
path_list <- dir(
  '1_raw/spatial/usfs_treemap/',
  pattern = '*.tif',
  full.names = TRUE
)
df_names <- path_list %>% 
  str_split_i('2016_', 2) %>% 
  str_remove('.tif') %>% 
  str_to_lower()

# Map over path list to run function on each one
reticulate::source_python('3_functions/spatial/raster_mean_by_polygon.py')
py_out <- map2(path_list, df_names, ~ raster_mean_by_polygon(county_path, .x, .y))
get_str(py_out)

# Combine DFs, rename columns, format variables
treemap_dat <- purrr::reduce(py_out, inner_join, by = 'fips') %>% 
  setNames(c(
    'fips',
    'forestCarbonLive',
    'forestCarbonDeadStanding',
    'forestCarbonDeadDown',
    'forestCanopyCover',
    'forestLiveTreeVolume',
    'forestLiveTrees',
    'forestDeadTrees',
    'forestStandHeight'
  )) %>% 
  pivot_longer(
    cols = !fips,
    names_to = 'variable_name',
    values_to = 'value'
  ) %>% 
  mutate(year = '2016')
get_str(treemap_dat)

# Save to results
results$treemap <- treemap_dat



## Metadata ----------------------------------------------------------------


get_str(treemap_dat)
vars <- treemap_dat$variable_name %>% 
  unique %>% 
  sort
vars

metas$treemap <- data.frame(
  variable_name = vars,
  metric = c(
    'Forest canopy cover',
    'Forest carbon - dead and down',
    'Forest carbon - standing dead',
    'Forest carbon - live standing',
    'Dead trees per forested acre',
    'Live trees per forested acre',
    'Forest live tree volume per acre',
    'Forest stand height'
  ),
  definition = c(
    'Mean live Canopy cover percentage derived from the Forest Vegatation Simulator',
    'Mean carbon (tons per acre) of woody material greater than 3 inches in diameter on the ground, and stumps and their roots greater than 3 inches in diameter. Estimated from models based on geographic area, forest type, and live tree carbon density (Smith and Heath 2008).',
    'Mean carbon, standing dead (tons per acre).',
    'Mean carbon, live above ground (tons per acre).',
    'Number of live trees (diamater > 5 inches) per acre',
    'Number of standing dead trees (diamater > 5 inches) per acre',
    'Mean volume, live, cubic feet per acre.',
    'Height of dominant trees, in feet, derivedf from the Forest Vegatation Simulator'
  ),
  axis_name = c(
    'Canopy Cover (%)',
    'Dead Down Carbon (tons / acre)',
    'Dead Standing Carbon (tons / acre)',
    'Live Standing (tons / acre)',
    'Live Trees / acre',
    'Dead Trees / acre',
    'Live Tree Volume (ft^3 / acre)',
    'Stand Height (ft)'
  ),
  dimension = "environment",
  index = 'biodiversity',
  indicator = c(
    'tree vigor',
    rep('above ground biomass', 3),
    rep('tree vigor', 4)
  ),
  units = c(
    'percentage',
    rep('tons / acre', 3),
    rep('number / acre', 2),
    'cubic feet / acre',
    'feet'
  ),
  scope = 'national',
  resolution = 'county',
  year = '2016',
  latest_year = '2016',
  updates = "8 years",
  source = 'TreeMap 2016: A tree-level model of the forests of the conterminous United States circa 2016',
  url = 'https://data.fs.usda.gov/geodata/rastergateway/treemap/index.php',
  citation = 'Riley, Karin L.; Grenfell, Isaac C.; Finney, Mark A.; Shaw, John D. 2021. TreeMap 2016: A tree-level model of the forests of the conterminous United States circa 2016. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2021-0074'
)

get_str(metas$treemap)


# Biofinder ---------------------------------------------------------------


# Explore layers
gdb_path <- '1_raw/spatial/biofinder/VCD2024.gdb'
(layers <- st_layers(gdb_path))

# Pull out all at once
bio_layers <- map(layers$name, ~ st_read(gdb_path, layer = .x)) %>% 
  setNames(c(layers$name))

# Take these two and save directly as map layers
saveRDS(
  bio_layers$Bio4_UncommonSpecies_Priority, 
  '2_clean/spatial/map_layers/biofinder_uncommon_spp.rds'
)
saveRDS(
  bio_layers$Bio4_RTESpecies_HP,
  '2_clean/spatial/map_layers/biofinder_rte_spp.rds'
)

rm(bio_layers)



# USFS IDS ------------------------------------------------------------


# Insect and Disease Dataset (IDS)
# raw <- st_read('1_raw/spatial/usfs/CONUS_Region9_2023.gdb/CONUS_Region9_2023.gdb/')

# What are we doing here. I don't remember. We didn't like this dataset



# CDL - CSV ---------------------------------------------------------------


# Here we can get multiple years, diversity across years?
# https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php#common.5
paths <- dir(
  '1_raw/nass/cropland_data_layer/County_Pixel_Count/',
  pattern = 'Acres.*csv',
  full.names = TRUE
)
year_name <- str_extract(paths, '[0-9]{4}')
dat <- map(paths, read_csv) %>% 
  setNames(c(year_name))

# Pull in cdl_key to rename columns. Add leading zeroes to match column names
cdl_key <- read_csv(
  '1_raw/nass/2023_30m_cdls/cdl_key.csv', 
  col_select = c(1, 2)
) %>% 
  mutate(code = sprintf("%03d", code))

# Pull in fips key to filter to New England
fips_key <- readRDS('5_objects/fips_key.rds')

# Rename and clean
cdl_clean <- map(dat, ~ {
  df <- .x %>% 
    select(fips = Fips, starts_with('Category')) %>% 
    mutate(fips = ifelse(str_length(fips) == 4, paste0('0', fips), fips)) %>% 
    filter(fips %in% fips_key$fips) %>% 
    setNames(c('fips', str_split_i(names(.)[-1], '_', 2))) %>% 
    setNames(c('fips', cdl_key$class[match(names(.)[-1], cdl_key$code)]))
  na_indices <- which(is.na(colnames(df)))
  colnames(df)[na_indices] <- letters[seq_along(na_indices)]
  return(df)
}) %>% 
  keep(~ nrow(.x) > 1)

# Now get diversity for each fips for each year
# First have to remove non-crop classes
pattern <- c('Developed|Forest|Shrubland|Grassland|Wetland|Barren|Missing|Water')
cdl_div <- imap(cdl_clean, ~ {
  .x %>% 
    column_to_rownames('fips') %>% 
    select(!matches(pattern)) %>% 
    diversity() %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    setNames(c('fips', 'cropDiversity')) %>% 
    mutate(year = .y) %>% 
    pivot_longer(
      cols = cropDiversity,
      names_to = 'variable_name',
      values_to = 'value'
    )
  }) %>% 
  bind_rows()
get_str(cdl_div)

# Save it
results$crop_div <- cdl_div



## Metadata ----------------------------------------------------------------


get_str(cdl_div)
years <- cdl_div$year %>% unique %>% sort %>% paste0(collapse = ', ')

metas$crop_div <- data.frame(
  variable_name = 'cropDiversity',
  metric = 'Crop diversity',
  definition = 'Shannon diversity index of crop types based on USDA Cropland Data Layer. Forests, grasslands, developed areas and open water were removed before calculations.',
  axis_name = 'Crop Diversity',
  dimension = 'production',
  index = 'production diversity',
  indicator = 'crop diversity',
  units = 'index',
  scope = 'national',
  resolution = 'county',
  year = years,
  latest_year = '2023',
  updates = "annual",
  source = 'U.S. Department of Agriculture, National Agricultural Statistics Service, Cropland Data Layer',
  url = 'https://www.nass.usda.gov/Research_and_Science/Cropland/SARS1a.php',
  citation = paste(
    'U.S. Department of Agriculture, National Agricultural Statistics Service, Cropland Data Layer: USDA NASS, USDA NASS Marketing and Information Services Office, Washington, D.C.',
    'Retrieved from:', 
    'https://www.nass.usda.gov/Research_and_Science/Cropland/Release/index.php',
    'December 14th, 2024'
  )
)

get_str(metas$crop_div)



# Save and Clear ----------------------------------------------------------


# Put metrics and metadata together into two single DFs
out <- aggregate_metrics(results, metas)

# Check record counts
check_n_records(out$result, out$meta, 'lulc')

saveRDS(out$result, '5_objects/metrics/lulc.RDS')
saveRDS(out$meta, '5_objects/metadata/lulc_meta.RDS')

# Also save spatial files as is
saveRDS(div, '5_objects/spatial/county_lulc_tables.rds')
saveRDS(hotspots, '2_clean/spatial/map_layers/hotspots.rds')
saveRDS(atlas, '2_clean/spatial/map_layers/atlas.rds')

clear_data()
