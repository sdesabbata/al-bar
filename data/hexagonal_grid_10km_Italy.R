# This script downloads a polygon for Italy from OpenStreetMap
# and generates exagonal grid of different sizes
# 
# OpenStreetMap® is open data, licensed under the Open Data Commons Open Database License (ODbL) 
# by the OpenStreetMap Foundation (OSMF) © OpenStreetMap contributors.
# https://www.openstreetmap.org/copyright
#
# Author: Stefano De Sabbata
# Date: 9 April 2022
#
# The code is partially based on examples from
# the osmextract GitHub repo
# the https://github.com/ropensci/osmextract


# Libraries ---------------------------------------------------------------

library(osmextract)
library(tidyverse)
library(lubridate)
library(sf)


# Data --------------------------------------------------------------------

# NOTE: the two instructions below download large files and 
# they might require confirmation, run separately
#
# osmextract warning:
# The input place was matched with: Italy
# You are trying to download a file from https://download.geofabrik.de/europe/italy-latest.osm.pbf
# This is a large file (1527 MB)!

# Retrieve all OSM multipolygons in Italy
osm_polygons <- 
  oe_get(
    "Italy", 
    layer = "multipolygons", 
    stringsAsFactors = FALSE, 
    quiet = FALSE,
    download_directory = "storage"
  )



# Polygon for Italy -------------------------------------------------------

boundary_Italy <-
  osm_polygons %>%
  filter(
    name == "Italia",
    type == "boundary",
    admin_level == 2
  )


# Hexagonal gird ----------------------------------------------------------

hex_10k_Italy <- 
  boundary_Italy %>% 
  st_transform(crs = 23032) %>% 
  st_make_grid(cellsize = 10000, square = FALSE) %>% 
  st_transform(crs = 4326) %>% 
  st_as_sf() %>% 
  rowid_to_column("hex_id")

hex_50k_Italy <- 
  boundary_Italy %>% 
  st_transform(crs = 23032) %>% 
  st_make_grid(cellsize = 50000, square = FALSE) %>% 
  st_transform(crs = 4326) %>% 
  st_as_sf() %>% 
  rowid_to_column("hex_id")


# Save --------------------------------------------------------------------

hex_10k_Italy %>% 
  write_sf("data/hexagonal_grid_10km_Italy.geojson")

hex_50k_Italy %>% 
  write_sf("data/hexagonal_grid_50km_Italy.geojson")

boundary_Italy %>% 
  write_sf("data/boundary-Italy.geojson")
