import os
import geopandas as gpd
import rasterio
import numpy as np
from rasterstats import zonal_stats

# Check environment
import sys
print(sys.prefix)

# check working directory
import os
os.getcwd()

# set working directory
os.chdir('c:\\Users\\cdonov12\\local\\repos\\sm-data')

# File paths
raster_path = '1_raw/spatial/usfs_treemap/TreeMap2016_CARBON_L.tif'
polygons_path = '5_objects/spatial/counties_shapefile.shp'

# Load the polygons as a GeoDataFrame
gdf = gpd.read_file(polygons_path)

# Shapefile crs
print(gdf.crs)

# Open the raster and check its CRS
with rasterio.open(raster_path) as src:
    raster_crs = src.crs
print(f"Raster CRS: {raster_crs}")
raster_crs

# Reproject shapefile to match raster
gdf_prj = gdf.to_crs(raster_crs)

# Verify the CRS of the reprojected shapefile
print(f"Reprojected Shapefile CRS: {gdf_prj.crs}")
print(gdf_prj.crs)

# Check equality
raster_crs == gdf_prj.crs

# Open the raster file and get the NoData value
with rasterio.open(raster_path) as src:
    nodata_value = src.nodata

# Compute zonal statistics
stats = zonal_stats(
    gdf_prj,   
    raster_path,
    stats="mean sum",
    geojson_out=True,
    nodata_value=nodata_value
)

# Convert results back into a GeoDataFrame
stats_gdf = gpd.GeoDataFrame.from_features(stats)

# Check it out
stats_gdf
stats_gdf.info()

# Save
os.getcwd()
stats_gdf.to_file("5_objects/spatial/aggregated_biomass/biomass_by_counties.shp")