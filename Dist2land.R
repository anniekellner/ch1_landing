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

load('ice_calc.RData')

albers.proj <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") # Albers Equal Area Conic (same as ArcMap doc)

land <- st_read('C:/Users/akell/Documents/ArcGIS/Land Shapefiles/AK_CA_5kbuff.shp')
land <- st_transform(land, albers.proj)

# Function to create sf object
DFtoSF <- function(df) {
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.alb <- st_transform(sf.wgs, albers.proj)
  return(sf.alb)
}

pb <- DFtoSF(ice)

# plot to make sure projections match and points look good

ggplot() +
  geom_sf(data = land, fill = "grey") +
  geom_sf(data = pb) 

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

dist <- vector()
dist <- st_distance(pb, land, by_element = TRUE)

ice$dist2land <- cbind(matrix(dist))

save(ice, file = 'ice_calc.RData')
