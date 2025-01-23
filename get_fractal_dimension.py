import os
import geopandas as gpd
import numpy as np
import pandas as pd
from scipy.stats import linregress



# Function to calculate fractal dimension
def fractal_dimension_from_perimeter_area(perimeters, areas):
    # Convert to numpy arrays and ensure all values are float
    perimeters = np.array(perimeters, dtype=np.float64)
    areas = np.array(areas, dtype=np.float64)

    # Filter out non-positive values
    valid_indices = (perimeters > 0) & (areas > 0)
    perimeters = perimeters[valid_indices]
    areas = areas[valid_indices]

   # if len(perimeters) == 0 or len(areas) == 0:
    #    raise ValueError("Perimeters and areas must contain positive values only.")

    # Calculate log values
    log_perimeters = np.log(perimeters)
    log_areas = np.log(areas)

    # Check if there's enough variation in the log areas to perform regression
    #if np.isclose(np.var(log_areas), 0):
     #   raise ValueError("Insufficient variation in area data for meaningful regression.")

    # Perform linear regression
    slope, intercept, r_value, p_value, std_err = linregress(log_areas, log_perimeters)
    
   # if np.isnan(slope):
    #    raise ValueError("Invalid computation resulting in NaN values.")

    # The slope is D/2, so multiply by 2 to get D
    fractal_dimension = 2 * slope
    return fractal_dimension, intercept, r_value, p_value, std_err

# Path to the directory containing the files
directory = '/Users/webbe/Documents/ALPOD/Lakes_clipped/'

# List to store results
results = []

# Loop through each file in the directory
for filename in os.listdir(directory):
    if filename.endswith('.shp'):  
        file_path = os.path.join(directory, filename)
        
        # Read the file into a pandas DataFrame
        data = gpd.read_file(file_path)
        
        perimeters = data['perim_km']
        areas = data['area_km2']
        
        # Calculate fractal dimension
        dimension, intercept, r_value, p_value, std_err = fractal_dimension_from_perimeter_area(perimeters, areas)
        
        # extract region
        parts = filename.split('_')
        
        # The second part should contain 'region.shp'
        second_part = parts[1]

        # Split the second part on '.' to remove the file extension
        name = second_part.split('.')[0]
        
        # Append the result to the list
        results.append({
            'Region': name,
            'Fractal Dimension': dimension,
            'Intercept': intercept,
            'R-squared': r_value**2,
            'P-value': p_value,
            'Std Error': std_err
        })

# Convert results to a DataFrame 
results_df = pd.DataFrame(results)

# Print the results
print(results_df)

# save the results to a new CSV file
results_df.to_csv('fractal_dimensions.csv', index=False)

print