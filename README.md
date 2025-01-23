## Weekly Sentinel-2-based surface water dataset creation (created using ALPOD workflow)
_ALPOD_create_extraction_shps.py_ takes input shapefiles of lakes, buffers by 60 m, subtracts rivers, dissolves overlapping polygons, and adds a lake_id. Output is used as an input for subsequent surface water occurance processing.  

_BatchLakeOccurence.ipynb_ Exports the weekly surface water occurance within each lake (buffered by 60 m). Exports the weekly occurance for each tile within each region. Needs to be run seperately for each region in each year.

_WeeklyMosaic.ipynb_ Takes weekly surface water occurance tiles (output from _BatchLakeOccurence.ipynb_), removes bad tiles (i.e., where surface water occurence is <50% of expected lake area and number of lakes is <30% of expected), and mosaics by week and region.

## Data curation 
_Landsat_lake_wise_export.ipynb_ Exports the maximum annual surface water extent within each lake polygon (buffered by 60 m) for the Pekel et al., (2016) and Pickens et al., (2020) products.

_S2_lake-wise_export.ipynb_ Exports the weekly surface water extent within each lake polygon (buffered by 60 m) for the Sentinel-2 based product (created using ALPOD workflow).

_get_fractal_dimension.py_ takes input shapefiles of lakes and outputs regional fractal dimensions  

## Data visualization, statistics, and summary 
_MakeLakeTable.py_ takes input shapefiles of lakes and input csv of fractal dimensions and outputs a summary table (Table 1)  

