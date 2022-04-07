# This scripts downloads all the points and polygons in Italy for OpenStreetMap
# and generates a dataset containing all the cafes.
# 
# OpenStreetMap® is open data, licensed under the Open Data Commons Open Database License (ODbL) 
# by the OpenStreetMap Foundation (OSMF) © OpenStreetMap contributors.
# https://www.openstreetmap.org/copyright
#
# Author: Stefano De Sabbata
# Date: 7 April 2022
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

# Retrieve all OSM points in Italy
osm_points <- 
  oe_get(
    "Italy", 
    layer = "points", 
    stringsAsFactors = FALSE, 
    quiet = FALSE,
    download_directory = "storage"
  )

# Retrieve all OSM multipolygons in Italy
osm_polygons <- 
  oe_get(
    "Italy", 
    layer = "multipolygons", 
    stringsAsFactors = FALSE, 
    quiet = FALSE,
    download_directory = "storage"
  )


# Extract cafes -----------------------------------------------------------

# If a specific region or city is needed
# the code below can be used to filter such area
# 
# boundary_Lombardia <-
#   osm_polygons %>% 
#   filter(
#     name == "Lombardia",
#     type == "boundary",
#     admin_level == 4
#   )
#
# your data... %>% 
#   st_filter(boundary_Lombardia)


# Extract cafe points
cafes_Italy_points <-
  osm_points %>% 
  filter(
    other_tags %>% 
      str_detect("\"amenity\"=>\"cafe\"")
  ) 

# Extract cafe polygons
cafes_Italy_polygons <-
  osm_polygons %>% 
  filter(amenity == "cafe")
    
# Combine points and polygons
# using polygons' centroid
cafes_Italy <-
  cafes_Italy_points %>% 
  select(osm_id, name, other_tags) %>% 
  mutate(original_geom = "point") %>% 
  bind_rows(
    cafes_Italy_polygons %>% 
      select(osm_id, name, other_tags) %>% 
      mutate(original_geom = "multipolygon") %>% 
      filter(st_is_valid(geometry)) %>%
      st_centroid()
  )


# Save data ---------------------------------------------------------------

cafes_Italy %>% 
  write_sf(
    paste0(
      "data/cafes-Italy-", 
      now() %>% 
        str_replace_all(" ", "-") %>% 
        str_replace_all(":", "-"),
      ".geojson"
    )
  )

rm(list = ls())
