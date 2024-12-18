import geopandas as gpd
from rasterstats import zonal_stats

def process_spatial(polygons_path, raster_path, output_path):
    # Load the polygons and raster files
    polygons = gpd.read_file(polygons_path)
    
    # Calculate the zonal statistics
    stats = zonal_stats(polygons, raster_path, stats="mean")
    
    # Add mean raster values to the GeoDataFrame
    polygons["mean_raster_value"] = [stat["mean"] for stat in stats]
    
    # Save the updated GeoDataFrame
    polygons.to_file(output_path)
    return output_path
