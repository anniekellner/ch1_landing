###############################################
#####   Distance to land    ###################
###############################################

# Calculate distance from land bears to land

rm(list = ls())

library(sf)
library(sp)
library(dplyr)
library(ggplot2)

# LOAD DATA --------------------------------------------------------------------------------- #

load('land_bears_ows.RData')

polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') 

land <- st_read('C:/Users/akell/Documents/ArcGIS/Land Shapefiles/AK_CA_5kbuff.shp')
land <- st_transform(land, polar.stereo)

# Function to create sf object
DFtoSF <- function(df) {
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.ps <- st_transform(sf.wgs, polar.stereo)
  return(sf.ps)
}

pb <- DFtoSF(land.bears.all.ows)

# plot to make sure projections match and points look good

ggplot() +
  geom_sf(data = land, fill = "grey") +
  geom_sf(data = pb) 

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

dist <- vector()
dist <- st_distance(pb, land, by_element = TRUE)

pb$dist2land <- cbind(matrix(dist))

save(pb, file = 'land_bears_ows.RData')

# -------------------------------------------------------------------------- #

# Verify

# land polygon is invalid
library(lwgeom)
land <- st_make_valid(land)

# check first ten points

test <- pb[1:10,]
test <- st_as_sf(test)

library(tmap)
tmap_mode("view")

land.map <- tm_shape(land) +
  tm_polygons() 

land.map +
tm_shape(test) + 
  tm_dots()
  

