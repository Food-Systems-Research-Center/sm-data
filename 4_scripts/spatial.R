# LULC
# 2025-05-26 update

# Land use diversity from MRLC, forest health and complexity from USDS TreeMap,
# Crop diversity from USDA Cropland Data Layer, biodiversity from NatureServe

# NOTE: we are dropping TreeMap2016 data for now. Hopefully getting newer 
# datasets soon.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  sf,
  stars,
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
  tidyr,
  readr
)

results <- list()
metas <- list()



# MRLC LULC ---------------------------------------------------------------
## 1. Download -------------------------------------------------------------


# Use shell script to download land use rasters by year
# THIS TAKES TIME TO RUN - only use it to refresh when needed

# system("bash 1_raw/spatial/mrlc_lulc/conus/curl_nlcd.sh")



## 2. Reduce ---------------------------------------------------------------


# Reduce them in size to neast, remove conus layers to save space
# This is also slow enough that we will comment it out until needed

# reticulate::source_python('3_functions/spatial/mask_rasters.py')
# mask_rasters(
#   input_dir = '1_raw/spatial/mrlc_lulc/conus',
#   output_dir = '1_raw/spatial/mrlc_lulc/neast',
#   aoi_path = '2_clean/spatial/neast_mask.gpkg'
# )



## 3. Calculate ------------------------------------------------------------


# Calculate stats by county in neast

## Get cell counts of each category for each county
county_path <- '2_clean/spatial/neast_counties_2024.gpkg'
state_path <- '2_clean/spatial/all_states.gpkg'
raster_paths <- list.files(
  path = '1_raw/spatial/mrlc_lulc/conus',
  pattern = '.tif$',
  full.names = TRUE
)

# Load python function, get cell counts for county and state
reticulate::source_python('3_functions/spatial/cat_zonal_stats.py')
out <- map(raster_paths, ~ cat_zonal_stats(.x, county_path))
get_str(out)
# ~ 6 minutes for county + state, but only 20 seconds for county in a single
# year. Let's just do counties and ditch state then.

# Save intermediate outputs
saveRDS(out, '5_objects/spatial/processing/neast_counties_zonal_out.rds')



## 4. Wrangle --------------------------------------------------------------


# Pull from intermediate outputs
out <- readRDS('5_objects/spatial/processing/neast_counties_zonal_out.rds')

# Get years onto them
out <- out %>% 
  setNames(c(paste0('y', 2015:2024)))
get_str(out)

# Get unique levels. Use later to make sure we are not missing any
all_levels <- map(out, names) %>% 
  unlist() %>% 
  unique() %>% 
  sort() %>% 
  str_subset('fips', negate = TRUE)

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


## Metadata ---------------------------------------------------------------


(vars <- meta_vars(results$lulc))

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
  meta_citation(date = '2024-11-15') %>% 
  select(-lulc_code)

get_str(metas$lulc_prop)



# LULC Diversity ---------------------------------------------------------------


# Shannon diversity of LULC by group. Lumping some categories into meaningful
# groups before getting LULC diversity. Groups:
#   all forest types together
#   grassland, pasture, and shrub
#   woody wetlands and herb wetlands
#   Development from 4 to 2: open space + low dev, medium + high dev
get_str(all_lulc)
lumped <- map(all_lulc, ~ {
  .x %>% 
    mutate(
      lulcForestLump = rowSums(across(contains('forest'))),
      lulcGrassLump = rowSums(across(contains('shrub|grassland|pasture'))),
      lulcWetlandLump = rowSums(across(contains('Wetlands'))),
      lulcDevLowLump = rowSums(across(contains('DevOpen|DevLow'))),
      lulcDevHighLump = rowSums(across(contains('DevMed|DevHigh'))),
    ) %>% 
    select(
      -matches('lulcPropDev|lulc.*forest$|PropShrub|PropGrass|PropPasture|Wetlands$')
    )
})
get_str(lumped)

div <- imap(lumped, ~ {
  data_year <- str_sub(.y, start = 2)
  .x %>% 
    column_to_rownames('fips') %>% 
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
    mutate(year = data_year)
}) %>% 
  bind_rows()
div

# Save
results$lulc_div <- div



### Metadata --------------------------------------------------------------

#   all forest types together
#   grassland, pasture, and shrub
#   woody wetlands and herb wetlands
#   Development from 4 to 2: open space + low dev, medium + high dev

metas$lulc_div <- data.frame(
  variable_name = 'lulcDiversity',
  metric = 'Land Use Diversity',
  definition = paste(
    'Shannon diversity of LULC proportions from MRLC Land Use Land Cover 30m layer by county.',
    'LULC Codes grouped into categories: developed low (dev open, dev low int), developed high (dev med int, dev high int),',
    'forest (mixed, evergreen, deciduous), grasslands (grassland, pasture, shrub),',
    'wetlands (herbaceous, woody), and the remaining categories: barren, open water, cultivated crops, barren.',
    'Larger numbers represent greater diversity of LULC.'
  ),
  axis_name = 'LULC Diversity',
  dimension = "environment",
  index = 'biodiversity',
  indicator = 'land use diversity',
  units = 'index',
  scope = 'national',
  resolution = '30m',
  year = meta_years(results$lulc_div),
  latest_year = meta_latest_year(results$lulc_div),
  updates = "annual",
  source = paste(
    'U.S. Geological Survey, Multi-Resolution Land Characteristics Consortium (2024).',
    'Sioux Falls, SD'
  ),
  url = 'https://www.mrlc.gov/data/type/land-cover'
) %>% 
  meta_citation(date = '2025-07-01')

get_str(metas$lulc_div)



# TreeMap 2016 ------------------------------------------------------------


# NOTE: Dropping this for now - waiting on newer datasets

# # This is just live carbon layer. Doing more below
# # https://www.fs.usda.gov/rds/archive/catalog/RDS-2021-0074
# treemap <- read_stars('1_raw/spatial/usfs_treemap/TreeMap2016_CARBON_L.tif')
# 
# # Crop
# ne_counties_prj <- st_transform(ne_counties, st_crs(treemap))
# treemap_crop <- st_crop(treemap, ne_counties_prj)
# 
# # Saving this raster to use straight up in Quarto
# # saveRDS(treemap_crop, '2_clean/spatial/map_layers/treemap_biomass.rds')



# Redo TreeMap ------------------------------------------------------------


# NOTE: dropping this for now, waiting on newer datasets

# # Run python script to aggregate TreeMap2016 data by counties
# path_list <- dir(
#   '1_raw/spatial/usfs_treemap/',
#   pattern = '*.tif',
#   full.names = TRUE
# )
# df_names <- path_list %>% 
#   str_split_i('2016_', 2) %>% 
#   str_remove('.tif') %>% 
#   str_to_lower()
# 
# # Load python function to get means of raster value within each polygon
# reticulate::source_python('3_functions/spatial/raster_mean_by_polygon.py')
# 
# # Map over path list to run function on each one
# county_path = '2_clean/spatial/ne_counties_2024.gpkg'
# counties_out <- map2(path_list, df_names, ~ raster_mean_by_polygon(county_path, .x, .y))
# get_str(counties_out)
# 
# # Do it for states as well. 
# # Note this one takes a while! Whole country raster. Maybe an hour?
# states_path = '2_clean/spatial/all_states.gpkg'
# get_time()
# states_out <- map2(path_list, df_names, ~ raster_mean_by_polygon(states_path, .x, .y))
# get_str(states_out)
# 
# # Save outputs for posterity
# list(states_out, counties_out) %>% 
#   saveRDS('5_objects/spatial/processing/treemap_aggregation_out.RDS')



# # Pick up saved dataset, then combine, rename, put in variable format
# treemap_agg <- readRDS('5_objects/spatial/processing/treemap_aggregation_out.RDS')
# treemap_dat <- map(treemap_agg, ~ {
#   .x %>% 
#     purrr::reduce(inner_join) %>% 
#     setNames(c(
#       'fips',
#       'forestCarbonLive',
#       'forestCarbonDeadStanding',
#       'forestCarbonDeadDown',
#       'forestCanopyCover',
#       'forestLiveTreeVolume',
#       'forestLiveTrees',
#       'forestDeadTrees',
#       'forestStandHeight'
#     )) %>% 
#     pivot_longer(
#       cols = !fips,
#       names_to = 'variable_name',
#       values_to = 'value'
#     ) %>% 
#     mutate(year = '2016')
# }) %>% 
#   bind_rows()
# get_str(treemap_dat)
# 
# # Save to results
# results$treemap <- treemap_dat



## Metadata ----------------------------------------------------------------


# get_str(treemap_dat)
# (vars <- get_vars(treemap_dat))
# 
# metas$treemap <- data.frame(
#   variable_name = vars,
#   metric = c(
#     'Forest canopy cover',
#     'Forest carbon - dead and down',
#     'Forest carbon - standing dead',
#     'Forest carbon - live standing',
#     'Dead trees per forested acre',
#     'Live trees per forested acre',
#     'Forest live tree volume per acre',
#     'Forest stand height'
#   ),
#   definition = c(
#     'Mean live Canopy cover percentage derived from the Forest Vegatation Simulator',
#     'Mean carbon (tons per acre) of woody material greater than 3 inches in diameter on the ground, and stumps and their roots greater than 3 inches in diameter. Estimated from models based on geographic area, forest type, and live tree carbon density (Smith and Heath 2008).',
#     'Mean carbon, standing dead (tons per acre).',
#     'Mean carbon, live above ground (tons per acre).',
#     'Number of live trees (diamater > 5 inches) per acre',
#     'Number of standing dead trees (diamater > 5 inches) per acre',
#     'Mean volume, live, cubic feet per acre.',
#     'Height of dominant trees, in feet, derivedf from the Forest Vegatation Simulator'
#   ),
#   axis_name = c(
#     'Canopy Cover (%)',
#     'Dead Down Carbon (tons / acre)',
#     'Dead Standing Carbon (tons / acre)',
#     'Live Standing (tons / acre)',
#     'Live Trees / acre',
#     'Dead Trees / acre',
#     'Live Tree Volume (ft^3 / acre)',
#     'Stand Height (ft)'
#   ),
#   dimension = "environment",
#   index = 'biodiversity',
#   indicator = c(
#     'tree vigor',
#     rep('above ground biomass', 3),
#     rep('tree vigor', 4)
#   ),
#   units = c(
#     'percentage',
#     rep('tons / acre', 3),
#     rep('number / acre', 2),
#     'cubic feet / acre',
#     'feet'
#   ),
#   scope = 'national',
#   resolution = 'county, state',
#   year = '2016',
#   latest_year = '2016',
#   updates = "8 years",
#   source = 'TreeMap 2016: A tree-level model of the forests of the conterminous United States circa 2016',
#   url = 'https://data.fs.usda.gov/geodata/rastergateway/treemap/index.php',
#   citation = 'Riley, Karin L.; Grenfell, Isaac C.; Finney, Mark A.; Shaw, John D. 2021. TreeMap 2016: A tree-level model of the forests of the conterminous United States circa 2016. Fort Collins, CO: Forest Service Research Data Archive. https://doi.org/10.2737/RDS-2021-0074'
# )
# 
# get_str(metas$treemap)



# Cropland Data Layer -----------------------------------------------------


# Rather than downloading rasters, we can just download a CSV from NASS where
# values have already been aggregated:
# https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php#common.5
paths <- dir(
  '1_raw/nass/cropland_data_layer/County_Pixel_Count/',
  pattern = 'Acres.*csv',
  full.names = TRUE
)
year_name <- str_extract(paths, '[0-9]{4}')
dat <- map(paths, read_csv) %>% 
  setNames(c(year_name))
get_str(dat)
get_str(dat[[1]])

# Pull in cdl_key to rename columns. Add leading zeroes to match column names
cdl_key <- read_csv(
  '1_raw/nass/2023_30m_cdls/cdl_key.csv', 
  col_select = c(1, 2)
) %>% 
  mutate(code = sprintf("%03d", code))

# Rename and clean
# Dropping the *_all categories to avoid double counting. Downside is that 
# it means the double crop acres will look like new species.
# Some columns are not covered by our key, so they have NA names. We are also 
# giving these random letter names so we can continue to calculate diversity
cdl_clean <- map(dat, ~ {
  df <- .x %>% 
    select(fips = Fips, starts_with('Category')) %>% 
    mutate(fips = ifelse(str_length(fips) == 4, paste0('0', fips), fips)) %>% 
    setNames(c('fips', str_split_i(names(.)[-1], '_', 2))) %>% 
    setNames(c('fips', cdl_key$class[match(names(.)[-1], cdl_key$code)]))
  na_indices <- which(is.na(colnames(df)))
  colnames(df)[na_indices] <- letters[seq_along(na_indices)]
  return(df)
}) %>% 
  keep(~ nrow(.x) > 1)
get_str(cdl_clean)
get_str(cdl_clean[['2024']])

# Now get diversity for each Northeast county for each year
# First have to remove non-crop classes. Matching pattern to find those:
pattern <- c('Developed|Forest|Shrubland|Grassland|Wetland|Barren|Missing|Water')

# Map through each year
cdl_div_counties <- imap(cdl_clean, ~ {
  # Reduce to Northeast counties
  df <- .x %>% 
    filter_fips(scope = 'counties')
  
  # Another check to make sure this doesn't break - give NULL if not in fips key
  if (any(df$fips %in% fips_key$fips)) {
    out <- df %>% 
      column_to_rownames('fips') %>% 
      select(!matches(pattern)) %>% # Remove non-crop codes
      diversity(index = 'shannon') %>%
      as.data.frame() %>% 
      rownames_to_column() %>% 
      setNames(c('fips', 'cropDiversity')) %>% 
      mutate(year = .y) %>% 
      pivot_longer(
        cols = cropDiversity,
        names_to = 'variable_name',
        values_to = 'value'
      )
  } else {
    out <- NULL
  }
  return(out)
  }) %>% 
  bind_rows()
get_str(cdl_div_counties)

# NOTE: dropping CDL div by state for now - not using it
# # Group by state (first two letters of fips) and get diversity for each state
# cdl_div_states <- imap(cdl_clean, ~ {
#   .x %>% 
#     mutate(fips = str_sub(fips, end = 2)) %>% 
#     group_by(fips) %>% 
#     summarize(across(everything(), sum)) %>% 
#     column_to_rownames('fips') %>%
#     select(!matches(pattern)) %>%
#     diversity() %>%
#     as.data.frame() %>%
#     rownames_to_column() %>%
#     setNames(c('fips', 'cropDiversity')) %>%
#     mutate(year = .y) %>%
#     pivot_longer(
#       cols = cropDiversity,
#       names_to = 'variable_name',
#       values_to = 'value'
#     )
#   }) %>% 
#   bind_rows()
# get_str(cdl_div_states)

# # Combine counties and states, save to results
# results$crop_div <- bind_rows(cdl_div_counties, cdl_div_states)
# get_str(results$crop_div)



## CDL Area ----------------------------------------------------------------


# Adding another variable to be a utility metric that is just the sum of acres
# in each county, to be used as a weighting variable. Here, we need to remove
# all *_all variables as well as the Dbl crop variables to prevent double
# counting of acres. Still removing non-crop categories as well
get_str(cdl_clean)
get_str(cdl_clean[[1]])

test <- cdl_clean[[17]] %>% 
  filter_fips(scope = 'counties')
get_str(test)

# Map through each year
cdl_area <- imap(cdl_clean, ~ {
  # Reduce to Northeast counties
  df <- .x %>% 
    filter_fips(scope = 'counties')
  
  # Process if at least some fips in Northeast
  if (any(df$fips %in% fips_key$fips)) {
    # Remove non-crops, dbl crops
    out <- df %>% 
      column_to_rownames('fips') %>% 
      select(!matches(pattern)) %>% 
      select(!starts_with('Dbl'))
  
    # Sum total acres in CDL model
    out <- out %>% 
      mutate(acres = rowSums(., na.rm = TRUE)) %>% 
      rownames_to_column('fips') %>% 
      select(fips, acres) %>% 
      setNames(c('fips', 'cdlAgAcres')) %>% 
      mutate(year = .y) %>% 
      pivot_longer(
        cols = cdlAgAcres,
        names_to = 'variable_name',
        values_to = 'value'
      )
  } else {
    out <- NULL
  }
  return(out)
  }) %>% 
  bind_rows()
get_str(cdl_area)

# Combine with above
results$cdl <- bind_rows(cdl_div_counties, cdl_area)



## Metadata ----------------------------------------------------------------


get_str(results$cdl)
meta_vars(results$cdl)

metas$cdl <- data.frame(
  variable_name = meta_vars(results$cdl),
  metric = c(
    'Acres under agriculture (CDL)',
    'Crop diversity'
  ),
  definition = c(
    'Sum total of agricultural acres per county from NASS Cropland Data Layer. Non-agricultural codes and double crops have been removed.',
    'Shannon diversity index on the NASS Cropland Data Layer after non-agricultural acres and double crops have been removed.'
  ),
  axis_name = c('Ag Acres', 'Crop Diversity'),
  dimension = c('util_dimension', 'production'),
  index = c('util_index', 'production diversity'),
  indicator = c('util_indicator', 'production species diversity'),
  units = c('acres', 'index'),
  scope = 'national',
  resolution = '30m',
  year = meta_years(results$cdl),
  latest_year = meta_latest_year(results$cdl),
  updates = "annual",
  source = 'U.S. Department of Agriculture, National Agricultural Statistics Service, Cropland Data Layer',
  url = 'https://www.nass.usda.gov/Research_and_Science/Cropland/SARS1a.php'
) %>% 
  meta_citation(date = '2025-07-02')
get_str(metas$cdl)



# NatureServe -------------------------------------------------------------


# Number of ecosystem by state (but also species counts)
# https://geohub-natureserve.opendata.arcgis.com/datasets/Natureserve::natureserve-biodiversity-in-focus-total-ecosystems/about
ns_raw <- st_read('1_raw/spatial/nature_serve/NatureServe_Biodiversity_in_Focus_-_Total_Ecosystems.geojson') %>% 
  janitor::clean_names()
ns_raw
get_str(ns_raw)

# Drop geometry to get plain df, then keep only relevant columns
# We want species counts, at risk species, ecosystem count, and at risk eco
ns <- st_drop_geometry(ns_raw) %>% 
  select(
    fips = state_fips,
    matches('^total|^atrisk'),
    atrisk_ecosystems = pct_atrisk_eco,
    -matches('sym$')
  )
get_str(ns)

# Filter down to 50 states and DC and add current year column
ns <- filter(ns, fips %in% state_key$state_code) %>% 
  mutate(year = 2022) %>% 
  select(fips, year, everything())
get_str(ns)

# Make better variable names
ns <- ns %>% 
  setNames(c(
    names(.)[1:2],
    'sppAnimals',
    'sppPlants',
    'sppBees',
    'sppOrchids',
    'pctAtRiskAnimalSpp',
    'pctAtRiskPlantSpp',
    'pctAtRiskBeeSpp',
    'pctAtRiskOrchidSpp',
    'nEcosystems',
    'pctAtRiskEcosystems'
  ))
get_str(ns)

# Add current year and pivot longer to fit with other metrics
# Make value character to match others
ns <- ns %>% 
  mutate(year = 2022) %>% 
  pivot_longer(
    cols = !c('fips', 'year'),
    values_to = 'value',
    names_to = 'variable_name'
  ) %>% 
  mutate(across(everything(), as.character))
get_str(ns)

# Save to results list
results$nature_serve <- ns



## Metadata ----------------------------------------------------------------


get_str(results$nature_serve)
meta_vars(results$nature_serve)

metas$nature_serve <- data.frame(
  variable_name = meta_vars(results$nature_serve),
  metric = c(
    'Total number of ecosystems',
    'Percentage of animal species at risk',
    'Percentage of bee species at risk',
    'Percentage of ecosystems at risk',
    'Percentage of orchid species at risk',
    'Percentage of plant species at risk',
    'Total number of animal species',
    'Total number of bee species',
    'Total number of orchid species',
    'Total number of plant species'
  ),
  definition = c(
    'Number of ecosystems represented in state based on the US National Vegetation Classification from 2022',
    'Percentage of animal species within state that are at risk of extinction (vulnerable, imperiled, critically imperiled, or possibly extinct)',
    'Percentage of bee species within state that are at risk of extinction (vulnerable, imperiled, critically imperiled, or possibly extinct)',
    'Percentage of ecosystems within state that are at risk (vulnerable, imperiled, or critically imperiled)',
    'Percentage of orchid species within state that are at risk of extinction (vulnerable, imperiled, critically imperiled, or possibly extinct)',
    'Percentage of plant species within state that are at risk of extinction (vulnerable, imperiled, critically imperiled, or possibly extinct)',
    'Total number of animal species within state',
    'Total number of bee species within state',
    'Total number of orchid species within state',
    'Total number of plant species within state'
  ),
  axis_name = c(
    'Number of Ecosystems',
    'At Risk Animal Spp (%)',
    'At Risk Bee Spp (%)',
    'At Risk Ecosystems (%)',
    'At Risk Orchid Spp (%)',
    'At Risk Plant Spp (%)',
    'Number Animal spp',
    'Number Bee spp',
    'Number Orchid spp',
    'Number Plant spp'
  ),
  dimension = 'environment',
  index = 'species and habitat',
  units = c(
    'count',
    rep('percentage', 5),
    rep('count', 4)
  ),
  scope = 'national',
  resolution = 'state',
  year = meta_years(results$nature_serve),
  latest_year = meta_latest_year(results$nature_serve),
  updates = "annual",
  source = 'NatureServe Network. (2023). Biodiversity in Focus: United States Edition. Arlington, VA.',
  url = 'https://geohub-natureserve.opendata.arcgis.com/datasets/Natureserve::natureserve-biodiversity-in-focus-total-ecosystems/about'
) %>% 
  meta_citation(date = '2025-02-13') %>% 
  mutate(indicator = case_when(
    str_detect(variable_name, 'Ecosystem') ~ 'sensitive or rare habitats',
    .default = 'biodiversity'
  ))

get_str(metas$nature_serve)



# Save and Clear ----------------------------------------------------------


# Put metrics and metadata together into two single DFs
out <- aggregate_metrics(results, metas)

# Check record counts
check_n_records(out$result, out$meta, 'spatial')

saveRDS(out$result, '5_objects/metrics/lulc.RDS')
saveRDS(out$meta, '5_objects/metadata/lulc_meta.RDS')

clear_data(gc = TRUE)
