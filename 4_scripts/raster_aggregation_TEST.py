# Aggregate TreeMap data county vectors

import rasterstats
import geopandas as gpd
import rasterio
# import rasterio.plt
import pandas as pd
import shapely
import numpy as np
import pandas as pd

# Load raster better
live_carbon = '1_raw/spatial/usfs_treemap/TreeMap2016_CARBON_L.tif'
with rasterio.open(live_carbon) as src:
    raster_crs = src.crs
live_carbon

# Load counties shapefile
county_path = '2_clean/spatial/ne_counties_2024.gpkg'
counties = gpd.read_file(county_path)
counties.info()

# Check crs
if counties.crs != raster_crs:
    counties = counties.to_crs(raster_crs)
counties.crs
raster_crs

# Calculate summary stats by county
lc_out = rasterstats.zonal_stats(
    counties,
    live_carbon,
    stats = ['mean'],
    geojson_out = True
)

# Convert to geodataframe
lc_gdf = gpd.GeoDataFrame.from_features(lc_out)
lc_gdf.info()

# Set crs
lc_gdf.crs
lc_gdf.set_crs(counties.crs, inplace=True)
lc_gdf.crs

# Save to gpkg to use in R
lc_gdf.to_file('5_objects/spatial/processing/test.gpkg')


## Get a regular dataframe out of it
lc_df = pd.DataFrame(lc_gdf)
lc_df = lc_df[['fips', 'mean']]


## Turn it into a function to process other rasters
def aggregate_raster(county_path, raster_path):
    
    # Get raster crs
    with rasterio.open(raster_path) as src:
        raster_crs = src.crs
    
    # Load counties shapefile
    counties = gpd.read_file(county_path)

    # Resolve CRS
    if counties.crs != raster_crs:
        counties = counties.to_crs(raster_crs)
    
    # Raster stats
    out = rasterstats.zonal_stats(county_path, raster_path, stats='mean')
    
    # Return a df
    df = gpd.GeoDataFrame.from_features(out)
    df.set_crs(counties.crs, inplace=True)
    df = pd.DataFrame(df)
    df = df[['fips', 'mean']]
    return df

## Test
county_path = '2_clean/spatial/ne_counties_2024.gpkg'
raster_path = '1_raw/spatial/usfs_treemap/TreeMap2016_CARBON_L.tif'

test = aggregate_raster(county_path, raster_path)
