### Data analysis, visualization, and summary 

_LandsatAnalysis_MLmodel.ipynb_    Fits the machine learning model relating the mean relative difference between products in dry years subtracted from the mean relative difference between products in wet years to various explantory features.

_LandsatComparison_Figure2.R_    Creates Figure 2 (Absolute and relative difference between lake area estimated by Landsat products and the Sentinel-2 product) and calculates the Spearmen's correlation coefficient and Fisher Z-transformation.

_LandsatComparison_Figure3.R_    Creates Figure 3 (Absolute difference between annual regional lake area estimated by Landsat and Sentinel-2 plotted by annual regional lake area) 

_LandsatComparison_Figure5.R_    Creates figure 5 (Lake area trends measured across regions with Sentinel-2 and Landsat products and effect of wetness sensitivity to the relative error in the trend) and calculates the average relative difference in the trend between Landsat products and S2.

_Table1.py_    takes input shapefiles of lakes and input csv of fractal dimensions and outputs a summary table (Table 1)  

_LandsatComparison_Table2.R_    Creates Table 2 (Regional lake area estimated by all products in dry and wet years) and calculates the percentage by which Landsat products over/under-estimate lake area in dry/wet years compared with S2.

_LandsatComparison_stats.R_    (1) Calcluates how much variance in the relative difference between Landsat trends and S2 trends is explained by wetness sensitivity. (2) Determines if regional lake area is higher in dryer years. (3) Summarizes absolute and relative differences between Landsat and S2. (4) Summarizes the percent differences between the Pekel and Pickens products.
