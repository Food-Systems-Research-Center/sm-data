import pandas as pd
import geopandas as gpd
import rasterstats

def cat_zonal_stats(raster_path, polygon_path):
  
  polygons = gpd.read_file(polygon_path)
  
  with rasterio.open(raster_path) as src:
    raster_crs = src.crs

  # Resolve CRS
  if polygons.crs != raster_crs:
    print('Transforming polygon CRS')
    polygons = polygons.to_crs(raster_crs)

  # Calculate frequency of each LULC class within each polygon
  out = rasterstats.zonal_stats(
    polygons,
    raster_path,
    stats=None,
    categorical=True
  )
  
  df = pd.DataFrame(out)
  df['fips'] = counties['fips']
  return df
