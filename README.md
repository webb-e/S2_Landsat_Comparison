## Weekly Sentinel-2-based surface water dataset creation 
The weekly S2-based surface water dataset was created following the workflow created by Levenson et al., (2025). [Code]([url](https://github.com/ericslevenson/ALPOD)), associated paper, and [archived dataset]([url](https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2399)). For this project, we used the vectorized lakes created by Levenson et al., (2025) in Alaska and created our own lake dataset following the same methods for the three Canadian study regions.

_ALPOD_create_extraction_shps.py_ takes input shapefiles of lakes, buffers by 60 m, subtracts rivers, dissolves overlapping polygons, and adds a lake_id. Output is used as an input for subsequent surface water occurance processing.  

_BatchLakeOccurence.ipynb_ Exports the weekly surface water occurance within each lake (buffered by 60 m). Exports the weekly occurance for each tile within each region. Needs to be run seperately for each region in each year.

_WeeklyMosaic.ipynb_ Takes weekly surface water occurance tiles (output from _BatchLakeOccurence.ipynb_), removes bad tiles (i.e., where surface water occurence is <50% of expected lake area and number of lakes is <30% of expected), and mosaics by week and region.

## Data curation 
_CloudOccurrence.ipynb_ Exports the annual cloudiness for each Sentinel-2 tile in each region of each year.

_Landsat_lake_wise_export.ipynb_ Exports the maximum annual surface water extent within each lake polygon (buffered by 60 m) for the Pekel et al., (2016) and Pickens et al., (2020) products.

_S2_lake-wise_export.ipynb_ Exports the weekly surface water extent within each lake polygon (buffered by 60 m) for the Sentinel-2 based product (created using ALPOD workflow).

_Lake_wise_cloudiness_export.ipynb_ Exports the mean annual cloudiness of each lake in the dataset. Cloudiness is determined from Sentinel-2.

_get_fractal_dimension.py_ takes input shapefiles of lakes and outputs regional fractal dimensions  

## Data analysis, visualization, and summary 
_MakeLakeTable.py_ takes input shapefiles of lakes and input csv of fractal dimensions and outputs a summary table (Table 1)  

_LandsatAnalysis_MLmodel.ipynb_ Fits the machine learning model relating the mean relative difference between products in dry years subtracted from the mean relative difference between products in wet years to various explantory features.

