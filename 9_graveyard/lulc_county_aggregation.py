# this is not working yet - can't get tables


import pandas as pd
import geopandas as gpd
import rasterstats

# Load LULC raster data
# lulc_raster = gpd.read_file('1_raw/spatial/mrlc_lulc/Annual_NLCD_LndCov_2023_CU_C1V0.tif')

# Load polygon data
county_path = '2_clean/spatial/ne_counties_2024.gpkg'
counties = gpd.read_file('2_clean/spatial/ne_counties_2024.gpkg')
# counties = gpd.GeoDataFrame(r.ne_counties_prj)

# Clip LULC raster data to extent of polygon data
# clipped_lulc = gpd.clip(lulc_raster, polygons)

with rasterio.open(raster_path) as src:
  raster_crs = src.crs

counties.crs    
raster_crs

# Resolve CRS
if counties.crs != raster_crs:
  print('Transforming county crs')
  counties = counties.to_crs(raster_crs)

# Calculate frequency of each LULC class within each polygon
out = rasterstats.zonal_stats(
  counties,
  # county_path,
  '1_raw/spatial/mrlc_lulc/Annual_NLCD_LndCov_2023_CU_C1V0.tif', 
  stats=['count'],
  geojson_out=True
)

# Convert stats to a GeoDataFrame
out_gdf = gpd.GeoDataFrame(out, geometry=polygons.geometry)
out_gdf = gpd.GeoDataFrame(out)

# Print the results
print(stats_gdf)

for county_stats in stats:
    county_name = county_stats['properties']['NAME']  # Replace with the correct column for county name
    counts = county_stats['properties']['count']  # This will give the total count of LULC cells
    print(f"County: {county_name}")
    print(f"Total LULC cells: {counts}")


# Step 1: Load the county shapefile as a GeoDataFrame
counties = gpd.read_file("path_to_county_shapefile.shp")

# Step 2: Define the raster file and the zone (county shapefile)
raster_file = "path_to_lulc_raster.tif"

# Step 3: Compute zonal statistics (counts of LULC classes per county)
stats = zonal_stats(counties, raster_file, stats="count", geojson_out=True)
