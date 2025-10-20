import pandas as pd
from pyproj import Transformer
import numpy as np

# Read your coordinates from a CSV
df = pd.read_csv('../r-data/nypd-stop/sqf-2024.csv')

# Replace '(null)' string values with NaN
df['STOP_LOCATION_X'] = pd.to_numeric(df['STOP_LOCATION_X'], errors='coerce')
df['STOP_LOCATION_Y'] = pd.to_numeric(df['STOP_LOCATION_Y'], errors='coerce')

# Use correct CRS for NYC State Plane (NAD83(2011), ftUS)
transformer = Transformer.from_crs("EPSG:6539", "EPSG:4326", always_xy=True)

# Create mask for valid coordinates (not NaN)
valid_coords = ~(df['STOP_LOCATION_X'].isna() | df['STOP_LOCATION_Y'].isna())

# Initialize the new columns with NaN
df['wgs84_lon'] = np.nan
df['wgs84_lat'] = np.nan

# Transform only the valid coordinates
if valid_coords.any():
    valid_x = df.loc[valid_coords, 'STOP_LOCATION_X'].values
    valid_y = df.loc[valid_coords, 'STOP_LOCATION_Y'].values
    
    # Note: X is easting, Y is northing, in feet
    transformed_lon, transformed_lat = transformer.transform(valid_x, valid_y)
    
    # Assign transformed coordinates back to the dataframe
    df.loc[valid_coords, 'wgs84_lon'] = transformed_lon
    df.loc[valid_coords, 'wgs84_lat'] = transformed_lat

# Print summary
print(f"Total rows: {len(df)}")
print(f"Rows with valid coordinates: {valid_coords.sum()}")
print(f"Rows with null coordinates: {(~valid_coords).sum()}")

# Save to new CSV
df.to_csv('output.csv', index=False)
print("Transformation complete. Output saved to 'output.csv'")