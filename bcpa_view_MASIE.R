#########################################
###   View BCPA   #######################
#########################################


library(sf)
library(dplyr)
library(sp)
library(tmap)

rm(list = ls())

# load data

load('all_v2.RData')
setwd('C:/Users/akell/Documents/ArcGIS') # GIS data

# Spatial Data

polar.stereo <- CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs')

land <- st_read('./Land Shapefiles/AK_CA_5kbuff.shp') # AK and Canada w/ 5K buffer
land <- st_transform(land, polar.stereo)
land_crop <- st_crop(land, st_bbox(ows))
                     
# Load MASIE from change points



# Function to turn df into sf (cannot droplevels in sf object so always need to start w df)

pb <- subset(all.v2, id == 'pb_06336.2006')

DFtoSF <- function(df) {
  df <- droplevels(df)
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.ps <- st_transform(sf.wgs, polar.stereo)
  return(sf.ps)
}

DFtoSF(pb)