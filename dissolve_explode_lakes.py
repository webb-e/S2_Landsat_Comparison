#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul  5 09:17:04 2024

@author: webbe
"""

import os 
import numpy as np
import geopandas as gpd
import matplotlib.pyplot as plt
import earthpy as et 
import pandas as pd
from pathlib import Path
import itertools
import string

import itertools
import string

def generate_combinations_with_prefix(prefix, length, count):
    characters = string.ascii_letters + string.digits
    combinations_with_prefix = []
    
    for comb in itertools.product(characters, repeat=length):
        if len(combinations_with_prefix) >= count:
            break
        combinations_with_prefix.append(prefix + ''.join(comb))
    
    return combinations_with_prefix


########################################################
############## TUK, MRD, AND ANDERSON PLAIN
########################################################

#############################
###### READ IN FILES
############################
## READ IN LAKE FILE
TUKMRDAND = gpd.read_file("/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/dissolved/MRD_TUK_Anderson_lakes_buffunion.shp")
## READ IN ROI files
roi_and = gpd.read_file("/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/ROIs/AND_roi.shp")
roi_mrd = gpd.read_file("/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/ROIs/MRD_roi.shp")
roi_tuk = gpd.read_file("/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/ROIs/TUK_roi.shp")
## READ IN RIVER FILE
Mriver = gpd.read_file("/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/River_vectors/MRD_rivers_drive30.shp")


#############################
###### MANIPULATE DATA
############################
## set crs
Mriver = Mriver.set_crs(roi_and.crs)

# reproject
target_crs = TUKMRDAND.crs
roi_and = roi_and.to_crs(target_crs)
roi_mrd = roi_mrd.to_crs(target_crs)
roi_tuk = roi_tuk.to_crs(target_crs)
Mriver = Mriver.to_crs(target_crs)
## clip to regions

TUKwithriver = TUKMRDAND.clip(roi_tuk)
MRDwithriver= TUKMRDAND.clip(roi_mrd)
ANDwithriver = TUKMRDAND.clip(roi_and)

### buffer and subtract rivers
Mriver['geometry'] = Mriver['geometry'].buffer(30)
TUKnoriver =gpd.overlay(TUKwithriver, Mriver, how='difference')
MRDnoriver = gpd.overlay(MRDwithriver, Mriver, how='difference')
ANDnoriver = gpd.overlay(ANDwithriver, Mriver, how='difference')

## get rid of multipolygons
TUK_exploeded = TUKnoriver.explode()
MRD_exploeded = MRDnoriver.explode()
AND_exploeded = ANDnoriver.explode()

### add a unique ID column
TUKID = generate_combinations_with_prefix('TUK', 6, len(TUK_exploeded))
MRDID = generate_combinations_with_prefix('MRD', 6, len(MRD_exploeded))
ANDID = generate_combinations_with_prefix('AND', 6, len(AND_exploeded))
TUK_exploeded['lake_id'] = TUKID
MRD_exploeded['lake_id'] = MRDID
AND_exploeded['lake_id'] = ANDID

### save to file
TUK_exploeded.to_file('/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/dissolved_with_ids/TUK_lakes.shp', driver='ESRI Shapefile')
MRD_exploeded.to_file('/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/dissolved_with_ids/MRD_lakes.shp', driver='ESRI Shapefile')
AND_exploeded.to_file('/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/dissolved_with_ids/AND_lakes.shp', driver='ESRI Shapefile')

#TUKepsg  = TUK.to_crs('EPSG:4326')

# AKCP
#AKCP = gpd.read_file("/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/CoastalPlain_lakes.shp")
#AKCPdissolved = AKCP.dissolve().simplify(25)
#AKCPdissolved.to_file('/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/dissolved/AKCP_dissolvedsimple.shp', driver='ESRI Shapefile')

# YKD = gpd.read_file("/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/YKDelta_lakes.shp")
# YKD = YKD.dissolve()#.explode()
# YKD.to_file('/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/dissolved/YKDelta_diss_noexplode.shp', driver='ESRI Shapefile')

# n_vertices=[] ###

# for i, row in YKD.iterrows():
#     # It's better to check if multigeometry
#    # multi = row.geometry.type.startswith("Multi")

#    # if multi:
#    #     n = 0
#         # iterate over all parts of multigeometry
#     #    for part in row.geometry:
#     #        n += len(part.exterior.coords)
#    # else:
#     n = len(row.geometry.exterior.coords)
#     n_vertices.append(n) ###


#YKD["n_vertices"] = n_vertices ###
#small = YKD[(YKD['n_vertices'] <= 1000000)]
#large = YKD[(YKD['n_vertices'] > 1000000)]
#large_simple = large.simplify(20)
#small.to_file('/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/dissolved/YKDelta_diss_small.shp', driver='ESRI Shapefile')###
#large.to_file('/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/dissolved/YKDelta_diss_large.shp', driver='ESRI Shapefile')###

#YKDsimple = YKD.dissolve().simplify(15)
#YKDsimple.to_file('/Users/webbe/Library/CloudStorage/GoogleDrive-webb.elizabeth.e@gmail.com/My Drive/PostDoc/ALPOD/Lake_shps_clipped/dissolved/YKDelta_diss_simple.shp', driver='ESRI Shapefile')
