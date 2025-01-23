## Comparison between Landsat-based surface water products and Sentinel-2 based surface water occurance
This is the companion code to 'Long-term lake area trends: how small, shallow lakes with emergent vegetation muddy the water'

### Data curation 
_CloudOccurrence.ipynb_ Exports the annual cloudiness for each Sentinel-2 tile in each region of each year.

_Landsat_lake_wise_export.ipynb_ Exports the maximum annual surface water extent within each lake polygon (buffered by 60 m) for the Pekel et al., (2016) and Pickens et al., (2020) products.

_S2_lake-wise_export.ipynb_ Exports the weekly surface water extent within each lake polygon (buffered by 60 m) for the Sentinel-2 based product (created using ALPOD workflow).

_Lake_wise_cloudiness_export.ipynb_ Exports the mean annual cloudiness of each lake in the dataset. Cloudiness is determined from Sentinel-2.

_postprocessing_lakes.ipynb_ Combines Landsat and Sentinel-2 lake area extent and applies filters. 

_ConcatRegions_cleandata.ipynb_ takes csvs of regional lake characteristics and annual surface water extent, combines them, and performs final filtering

_get_fractal_dimension.py_ takes input shapefiles of lakes and outputs regional fractal dimensions  

### Data analysis, visualization, and summary 
_MakeLakeTable.py_ takes input shapefiles of lakes and input csv of fractal dimensions and outputs a summary table (Table 1)  

_LandsatAnalysis_MLmodel.ipynb_ Fits the machine learning model relating the mean relative difference between products in dry years subtracted from the mean relative difference between products in wet years to various explantory features.

