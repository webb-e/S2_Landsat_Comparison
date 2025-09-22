### Data analysis, visualization, and summary 

_LandsatAnalysis_MLmodel.ipynb_  Fits the machine learning model relating the mean relative difference between products in dry years subtracted from the mean relative difference between products in wet years to various explantory features.

_LandsatComparison_Figure2.R_    Creates Figure 2 (Relative density distribution of lake sizes by study region)

_LandsatComparison_Figure3.R_    Creates Figure 3 (Regional interannual variation in annual maximum lake area) 

_LandsatComparison_Figure4.R_    Creates Figure 4 (Absolute and relative difference between lake area estimated by Landsat products and the Sentinel-2 product) and calculates the Spearmen's correlation coefficient and Fisher Z-transformation.

_LandsatComparison_Figure5.R_    Creates Figure 5 (Relative difference between estimates of maximum lake area) and fits an ANOVA to test if dryness sensitivity is different between comparisons

_LandsatComparison_Figure6.R_    Creates Figure 6 (Example lakes showing how Landsat and Sentinel-2 surface water classifications vary between wet and dry years)

_LandsatComparison_Figure7.R_    Creates Figure 7 (Lake area trends measured across regions with Sentinel-2 and Landsat products and effect of wetness sensitivity to the relative error in the trend) and calculates the average relative difference in the trend between Landsat products and S2.

_Table1.py_    takes input shapefiles of lakes and input csv of fractal dimensions and outputs a summary table (Table 1)  

_LandsatComparison_Table2.R_    Creates Table 2 (Regional lake area estimated by all products in dry and wet years) and calculates the percentage by which Landsat products over/under-estimate lake area in dry/wet years compared with S2.

_LandsatComparison_stats.R_    (1) Calcluates how much variance in the relative difference between Landsat trends and S2 trends is explained by wetness sensitivity. (2) Determines if regional lake area is higher in dryer years. (3) Summarizes absolute and relative differences between Landsat and S2. (4) Summarizes the percent differences between the Pekel and Pickens products.
