import rasterstats
import geopandas as gpd
import rasterio
import pandas as pd
import os
from functools import reduce

## Function to aggregate raster data by county
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
    out = rasterstats.zonal_stats(
      counties, 
      raster_path, 
      stats='mean',
      geojson_out=True
    )
    
    # Return a df
    df = gpd.GeoDataFrame.from_features(out)
    df.set_crs(counties.crs, inplace=True)
    df = pd.DataFrame(df)
    df = df[['fips', 'mean']]
    return df


## Paths for counties and all the rasters we want 
county_path = '2_clean/spatial/ne_counties_2024.gpkg'
path_list = [
    '1_raw/spatial/usfs_treemap/TreeMap2016_CARBON_L.tif',
    '1_raw/spatial/usfs_treemap/TreeMap2016_CARBON_D.tif',
    '1_raw/spatial/usfs_treemap/TreeMap2016_CARBON_DWN.tif',
    '1_raw/spatial/usfs_treemap/TreeMap2016_CANOPYPCT.tif',
    '1_raw/spatial/usfs_treemap/TreeMap2016_VOLCFNET_L.tif',
    '1_raw/spatial/usfs_treemap/TreeMap2016_TPA_DEAD.tif',
    '1_raw/spatial/usfs_treemap/TreeMap2016_TPA_LIVE.tif',
    '1_raw/spatial/usfs_treemap/TreeMap2016_STANDHT.tif'
]

## Run function for each raster, and rename column to match
dfs = []
for raster_path in path_list:
    # Get name for column
    base_name = os.path.basename(raster_path).replace('.tif', '')
    
    # Run aggregation function
    df = aggregate_raster(county_path, raster_path)
    
    # Rename the 'mean' column
    df.rename(columns={'mean': f'mean_{base_name}'}, inplace=True)
    
    # Append the renamed DataFrame to the results list
    dfs.append(df)


## Join all the DFs by the fips column
py_out = reduce(lambda left, right: pd.merge(left, right, on='fips', how='inner'), dfs)
