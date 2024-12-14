import pandas as pd
import geopandas as gpd
import rasterstats

def cat_zonal_stats(raster_path, polygon_path):
  
  # Load polygon file
  polygons = gpd.read_file(polygon_path)
  
  # Save CRS from raster as object without loading raster
  with rasterio.open(raster_path) as src:
    raster_crs = src.crs

  # If CRS is different, transform polygons to same crs as raster
  if polygons.crs != raster_crs:
    print('Transforming polygon CRS')
    polygons = polygons.to_crs(raster_crs)

  # Calculate frequency of each LULC class within each polygon
  # Note that we use polygon file, but only raster path
  out = rasterstats.zonal_stats(
    polygons,
    raster_path,
    stats=None,
    categorical=True
  )
  
  # Convert to dataframe, put add fips column back in
  df = pd.DataFrame(out)
  df['fips'] = counties['fips']
  return df
