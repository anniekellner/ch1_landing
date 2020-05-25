###############################################
#####   Distance to land    ###################
###############################################

# Calculate distance from land bears to land

rm(list = ls())

library(sf)
library(sp)
library(dplyr)
library(ggplot2)
library(tmap)
library(lwgeom)
library(proj4)

# LOAD DATA --------------------------------------------------------------------------------- #

load('land_bears_ows.RData')

polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') 

land <- st_read('C:/Users/akell/Documents/ArcGIS/Land Shapefiles/AK_CA_5kbuff.shp')
land.crop <- st_crop(land, xmin = -596311, ymin = 2083889, xmax = 850962, ymax = 3576142) # crop shapefile
land.ps <- st_transform(land.crop, polar.stereo)

# Verify land looks good

land.map <- tm_shape(land.ps) + 
  tm_polygons()



# Verify land and points look good

land.map +
  tm_shape(pb) + 
  tm_dots()

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

dist <- vector()
dist <- st_distance(pb, land.ps, by_element = TRUE)

pb.ps$dist2land <- cbind(matrix(dist))

save(pb, file = 'land_bears_ows.RData')

# -------------------------------------------------------------------------- #

# check first ten points

test <- pb[1:10,]


library(tmap)
tmap_mode("view")

land.map <- tm_shape(land.ps) +
  tm_polygons() 

land.map +
tm_shape(test) + 
  tm_dots(col = 'day')
  

