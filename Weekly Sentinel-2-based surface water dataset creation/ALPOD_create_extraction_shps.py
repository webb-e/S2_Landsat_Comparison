#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jul 16 13:23:50 2024

@author: webbe
"""

import geopandas as gpd
import itertools
import string
import networkx as nx

#############################
###### DEFINE GLOBAL VARIABLES BASED ON ROI
############################

region_name = None
riverfile = None
region_label = None
def regionfun(region):
  global region_name,  riverfile, region_label
  if region == 'TUK':
    region_name = 'TUKMRDAND'
    riverfile = "/Users/webbe/Documents/ALPOD/River_vectors/MRD_rivers_drive30.shp"
    region_label = 'TUK'
  elif region == 'AND':
    region_name = 'TUKMRDAND'
    riverfile = "/Users/webbe/Documents/ALPOD/River_vectors/MRD_rivers_drive30.shp"
    region_label = 'AND'
  elif region == 'MRD':
    region_name = 'TUKMRDAND'
    riverfile ="/Users/webbe/Documents/ALPOD/River_vectors/MRD_rivers_drive30.shp"
    region_label = "MRD"
  elif region == 'AKCP':
    region_name = 'AKCP'
    riverfile = "/Users/webbe/Documents/ALPOD/River_vectors/AKCP_rivers_30.shp"
    region_label = 'AKCP'
  elif region == 'YKD':
    region_name = 'YKD'
    riverfile = "/Users/webbe/Documents/ALPOD/River_vectors/YKD_rivers_drive_30.shp"
    region_label = "YKD"
  elif region == 'YKF':
    region_name = 'YKF'
    riverfile = "/Users/webbe/Documents/ALPOD/River_vectors/YKF_river.shp"
    region_label = "YKF"
  else:
    print("Invalid region")


#############################
###### READ IN FILES, DEAL WITH CRSs, AND CLIP TO ROIS
############################
regionfun('AND') # options = TUK, MRD, AND, AKCP, YKD, YKF

## READ IN FILES
lakeshpfile = '/Users/webbe/Documents/ALPOD/Lakes_clipped/justlakes_' + region_label + '.shp'
lakeshps = gpd.read_file(lakeshpfile)
river = gpd.read_file(riverfile)

## DEAL WITH CRSs
if region_name == 'TUKMRDAND':
    roi = gpd.read_file('/Users/webbe/Documents/ALPOD/ROIs/TUK_roi.shp')
    river = river.set_crs(roi.crs)
    TUKMRD_crs = lakeshps.crs
    river = river.to_crs(TUKMRD_crs)
else:
    AK_CRS = lakeshps.crs
    river = river.to_crs(AK_CRS)

#############################
###### BUFFER AND REMOVE RIVERS
############################

## buffer by 60 m
lakeshps['geometry'] = lakeshps['geometry'].buffer(60)

###buffer rivers 
river['geometry'] = river['geometry'].buffer(30)

# subtract rivers
region_no_river =gpd.overlay(lakeshps, river, how='difference')

#############################
###### IDENTIFY OVERLAPPING LAKE POLYGONS, DISSOLVE, AND SUM AREA AND PERIMETERS
############################

region_no_river['geometry'] = region_no_river.geometry.buffer(0)  # Clean geometry
spatial_index = region_no_river.sindex

def find_touches(idx, geom):
    possible_matches_index = list(spatial_index.intersection(geom.bounds))
    possible_matches = region_no_river.iloc[possible_matches_index]
    precise_matches = possible_matches[possible_matches.intersects(geom)]
    precise_matches = precise_matches[precise_matches.index != idx]
    return precise_matches.index.tolist()

# Apply find_touches function 
region_no_river['touches'] = region_no_river.apply(lambda row: find_touches(row.name, row.geometry), axis=1)


# Create a graph where nodes are polygons and edges exist between touching polygons
G = nx.Graph()
for idx, row in region_no_river.iterrows():
    G.add_node(idx)  # Ensure each polygon is a node in the graph
    for neighbor in row['touches']:
        G.add_edge(idx, neighbor)

# Find connected components (polygon chains)
components = list(nx.connected_components(G))

# Assign a chain identifier to each polygon
chain_id = {}
for chain_num, component in enumerate(components):
    for idx in component:
        chain_id[idx] = chain_num
        
region_no_river['chain_id'] = region_no_river.index.map(chain_id).fillna(len(components))

## add a column to count the number of lakes in the dissolved polygon
region_no_river['n_lakes'] = 1

# dissolve on the polygon_group
dissolved_intersects = region_no_river.dissolve(
    by = "chain_id",
    aggfunc= {"n_lakes" : 'sum',
              "area_km2" : 'sum',
              'perim_km': 'sum'})
dissolved_intersects.geometry = dissolved_intersects.geometry.buffer(0)
### explode any multigeometries
exploded = dissolved_intersects.explode()


#############################
###### ADD A REGION AND UNIQUE ID COLUMN
############################
def generate_combinations_with_prefix(prefix, length, count):
    characters = string.ascii_letters + string.digits
    combinations_with_prefix = []
    
    for comb in itertools.product(characters, repeat=length):
        if len(combinations_with_prefix) >= count:
            break
        combinations_with_prefix.append(prefix + ''.join(comb))
    
    return combinations_with_prefix


ID = generate_combinations_with_prefix(region_label, 6, len(exploded))
exploded['lake_id'] = ID
exploded['region'] = region_label

#############################
###### SAVE TO FILE
############################
savepath = '/Users/webbe/Documents/ALPOD/Lake_extraction_shps/' + region_label + '_extraction.shp'
exploded.to_file(savepath, driver='ESRI Shapefile')

