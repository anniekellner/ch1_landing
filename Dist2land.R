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

# Verify

land.map <- tm_shape(land) + 
  tm_polygons()

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")   # albers eq. area 55-65 lat


# Function to create sf object
DFtoSF <- function(df, projection) {
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.project <- st_transform(sf.wgs, projection)
  return(sf.project)
}
pb <- DFtoSF(pb, projection)

# Verify

land.map +
  tm_shape(pb) + 
  tm_dots()

# crop land shapefile

land.crop <- st_crop(land, xmin = -596311, ymin = 2083889, xmax = 850962, ymax = 3576142)


# plot to make sure projections match and points look good

ggplot() +
  geom_sf(data = land.crop, fill = "grey") +
  geom_sf(data = pb) 

land.ps <- st_transform(land.crop, polar.stereo)
pb.ps <- st_transform(pb, polar.stereo)

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

dist <- vector()
dist <- st_distance(pb.ps, land.ps, by_element = TRUE)

pb.ps$dist2land <- cbind(matrix(dist))

save(pb, file = 'land_bears_ows.RData')

# -------------------------------------------------------------------------- #

# check first ten points

test <- pb.ps[356:35655,]
test <- st_as_sf(test)

library(tmap)
tmap_mode("view")

land.map <- tm_shape(land.ps) +
  tm_polygons() 

land.map +
tm_shape(test) + 
  tm_dots()
  

