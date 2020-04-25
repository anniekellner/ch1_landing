######################################################
#########   EXPLORE ICE BEARS   ######################
######################################################

library(sf)
library(dplyr)
library(sp)
library(ggplot2)
library(tmap)

rm(list = ls())

# load data

load('all_v2.RData')
setwd('C:/Users/akell/Documents/ArcGIS') # GIS data

# ------------------------------------------------------------------------------------- #

# projection

albers.proj <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") # Albers Equal Area Conic (same as ArcMap doc)

# Create land bear & ice bear distinction in database

#ows.land$id.datetime <- paste(ows.land$id, ows.land$datetime, sep = " ")
#all.v2$land_bear_ows <- ifelse(all.v2$id.datetime %in% ows.land$id.datetime, 1, 0) # column for land bear during ows
#save(all.v2, file = 'all_v2.RData')

# Spatial Data

land <- st_read('./Land Shapefiles/AK_CA_5kbuff.shp') # AK and Canada w/ 5K buffer
land <- st_transform(land, albers.proj)
land_crop <- st_crop(land, st_bbox(ows))
#plot(st_geometry(land_crop))


# plot to make sure projections match and points look good

#ggplot() +
  #geom_sf(data = ows) +
  #geom_sf(data = land_crop, fill = "grey") 

# Function to turn df into sf (cannot droplevels in sf object so always need to start w df)

DFtoSF <- function(df) {
  df <- droplevels(df)
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.alb <- st_transform(sf.wgs, albers.proj)
  return(sf.alb)
}

den <- subset(all.v2, DenYr == 1 & ows == 1)

bears <- DFtoSF(den)

# tmap

tmap_mode("view") # in view mode, land fills in

tm_shape(bears) +
  tm_symbols(col = "animal")
  




  