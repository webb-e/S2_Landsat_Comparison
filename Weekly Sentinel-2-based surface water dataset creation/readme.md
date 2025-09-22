### Weekly Sentinel-2-based surface water dataset creation 
The weekly S2-based surface water dataset was created following the workflow created by Levenson et al., (2025). [Code](https://github.com/ericslevenson/ALPOD), [associated paper](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2024GL112771), and [archived dataset](https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2399). For this project, we used the vectorized lakes created by Levenson et al., (2025) in Alaska and created our own lake dataset following the same methods for the three Canadian study regions.

_ALPOD_create_extraction_shps.py_ takes input shapefiles of lakes, buffers by 60 m, subtracts rivers, dissolves overlapping polygons, and adds a lake_id. Output is used as an input for subsequent surface water occurance processing.  

_BatchLakeOccurence.ipynb_ Exports the weekly surface water occurance within each lake (buffered by 60 m). Exports the weekly occurance for each tile within each region. Needs to be run seperately for each region in each year.

_WeeklyMosaic.ipynb_ Takes weekly surface water occurance tiles (output from _BatchLakeOccurence.ipynb_), removes bad tiles (i.e., where surface water occurence is <50% of expected lake area and number of lakes is <30% of expected), and mosaics by week and region.
