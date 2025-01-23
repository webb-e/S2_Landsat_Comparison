import geopandas as gpd
import math
import pandas as pd

# Define regions and associated files
regions = {
    'TUK': {
        'riverfile': "/Documents/ALPOD/River_vectors/MRD_rivers_drive30.shp",
        'region_label': 'TUK'
    },
    'AND': {
        'riverfile': "/Documents/ALPOD/River_vectors/MRD_rivers_drive30.shp",
        'region_label': 'AND'
    },
    'MRD': {
        'riverfile': "/Documents/ALPOD/River_vectors/MRD_rivers_drive30.shp",
        'region_label': 'MRD'
    },
    'AKCP': {
        'riverfile': "/Documents/ALPOD/River_vectors/AKCP_rivers_30.shp",
        'region_label': 'AKCP'
    },
    'YKD': {
        'riverfile': "/Documents/ALPOD/River_vectors/YKD_rivers_drive_30.shp",
        'region_label': 'YKD'
    },
    'YKF': {
        'riverfile': "/Documents/ALPOD/River_vectors/YKF_river.shp",
        'region_label': 'YKF'
    }
}

# Load the fractal dimension data
fractaldf = pd.read_csv('ALPOD/fractal_dimensions.csv')

# Initialize an empty list to collect data for all regions
all_regions_summary = []

# Iterate over all regions
for region, data in regions.items():
    region_label = data['region_label']
    riverfile = data['riverfile']
    
    # Load the lake shapefile for the region
    lakeshpfile = f'ALPOD/Lakes_clipped/justlakes_{region_label}.shp'
    lakeshps = gpd.read_file(lakeshpfile)
    
    # Get the fractal dimension for the region
    fractal_dimension = fractaldf.loc[fractaldf['Region'] == region_label, 'Fractal Dimension'].values[0]
    
    # Calculate shoreline complexity
    denominator = 2 * math.sqrt(math.pi) * lakeshps['area_km2'] ** (fractal_dimension / 2)
    lakeshps['shoreline_complexity'] = lakeshps['perim_km'] / denominator
    
    # Generate the summary table for the region
    summary_table = {
        'region': region_label,
        'area_mean': lakeshps['area_km2'].mean(),
        'area_std': lakeshps['area_km2'].std(),
        'area_median': lakeshps['area_km2'].median(),
        'area_iqr': lakeshps['area_km2'].quantile(0.75) - lakeshps['area_km2'].quantile(0.25),
        'perimeter_mean': lakeshps['perim_km'].mean(),
        'perimeter_std': lakeshps['perim_km'].std(),
        'perimeter_median': lakeshps['perim_km'].median(),
        'perimeter_iqr': lakeshps['perim_km'].quantile(0.75) - lakeshps['perim_km'].quantile(0.25),
        'shoreline_complexity_mean': lakeshps['shoreline_complexity'].mean(),
        'shoreline_complexity_std': lakeshps['shoreline_complexity'].std(),
        'shoreline_complexity_median': lakeshps['shoreline_complexity'].median(),
        'shoreline_complexity_iqr': lakeshps['shoreline_complexity'].quantile(0.75) - lakeshps['shoreline_complexity'].quantile(0.25),
        'number_of_lakes': lakeshps['area_km2'].count(),
        'total_area': lakeshps['area_km2'].sum()
    }
    
    # Append to the combined list
    all_regions_summary.append(summary_table)

# Combine all regions into a single DataFrame
combined_summary = pd.DataFrame(all_regions_summary)

# Save the combined summary table to a CSV file
combined_savepath = 'Documents/ALPOD/all_regions_lakestats.csv'
combined_summary.to_csv(combined_savepath, index=False)
print(f"Saved summary table for all regions to {combined_savepath}")
