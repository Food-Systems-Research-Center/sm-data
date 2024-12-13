import pandas as pd
import geopandas as gpd
import rasterstats

# Load LULC raster data
# raster_path = gpd.read_file('1_raw/nass/2023_30m_cdls/2023_30m_cdls.tif')
raster_path = '1_raw/nass/2023_30m_cdls/2023_30m_cdls.tif'

# Load polygon data
county_path = '2_clean/spatial/ne_counties_2024.gpkg'
counties = gpd.read_file('2_clean/spatial/ne_counties_2024.gpkg')
# counties = gpd.GeoDataFrame(r.ne_counties_prj)

# Clip LULC raster data to extent of polygon data
# clipped_lulc = gpd.clip(lulc_raster, polygons)

with rasterio.open(raster_path) as src:
  raster_crs = src.crs


# Resolve CRS
if counties.crs != raster_crs:
  print('Transforming county crs')
  counties = counties.to_crs(raster_crs)

counties.crs    
raster_crs

# Calculate frequency of each LULC class within each polygon
out = rasterstats.zonal_stats(
  counties,
  '1_raw/nass/2023_30m_cdls/2023_30m_cdls.tif',
  stats=None,
  categorical=True
  # geojson_out=True
)

# Convert stats to a GeoDataFrame
# out_gdf = gpd.GeoDataFrame(out, geometry=polygons.geometry)
out_gdf = gpd.GeoDataFrame(out)

# Try as df
df = pd.DataFrame(out_gdf)
df = df.properties.to_frame()

## thing
df = pd.DataFrame(out)
df = pd.concat([counties['fips'], df], axis=1)
df.to_csv("testtesttestest.csv", index=False)

# Print the results
print(out_gdf)

for county_stats in out_gdf:
    county_name = county_stats['properties']['NAME']
    counts = county_stats['properties']['count']
    print(f"County: {county_name}")
    print(f"Total LULC cells: {counts}")


# Step 1: Load the county shapefile as a GeoDataFrame
counties = gpd.read_file("path_to_county_shapefile.shp")

# Step 2: Define the raster file and the zone (county shapefile)
raster_file = "path_to_lulc_raster.tif"

# Step 3: Compute zonal statistics (counts of LULC classes per county)
stats = zonal_stats(counties, raster_file, stats="count", geojson_out=True)
