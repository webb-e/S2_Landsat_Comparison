## Comparison between Landsat-based surface water products and Sentinel-2 based surface water occurance
This is the companion code to 'Long-term lake area trends: how small, shallow lakes with emergent vegetation muddy the water'

This repository is organized into folders as follows:

`Weekly Sentinel-2-based surface water dataset creation` contains the code used to create a dataset of weekly surface water occurance from Sentinel-2 reflectance values. This dataset was subsequently used to determine the annual maximum lake area for all lakes in the study regions.

`data curation` containes the code that takes existing raster data (e.g., Landsat surface water products, weekly Sentinel-2 surface water occurance) and creates the csv files used for analayis. This folder also contains code that applies filters (e.g., remove lakes without observations in each year) and concats regional csvs.

`Data analysis, visualization, and summary` contains code used to analyze, visualize, and summarize the data. The file 'Landsat_analysis_data.csv' is archived here.
