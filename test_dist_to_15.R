##############################################
## TEST: DISTANCE TO 15% SIC #################
##############################################

# Calculate bear's distance to pack ice ###
# Reclassify all ice >=15% with a single value
# use clump function (directions = 8) to identify clumps
# reclassify clumps so that only clumps with pixels > 10 are retained

rm(list = ls())

library(raster)
library(sp)
library(rgeos)
library(sf)
library(dplyr)
library(tidyr)

# load data

r <- raster('./SIC-TIFs/SIC_univ_Bremen/pb_06817/asi-n6250-20060821-v5.4.tif')
load('ice_calc.RData')
pb <- subset(ice.calc, animal=='pb_06817')

########### 
# Create spatial object

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) 

#############
# Prep raster

rc <- reclassify(r, c(1,15,0, 15,100,1, 100,255,0)) # Reclassify raster so SIC>15 = 1 and all else = 0

rcc <- clump(rc, directions=8) # all cells adjacent to main pack ice with 15% SIC or greater are considered part of ice pack
plot(rcc)

#writeRaster(rcc, './Tests/rc.clump.grd') #see which value corresponds to ice pack (89)
'+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0'
#################
#### Distance ###

pb.sf <- st_as_sf(pb.spdf.polar) #convert to sf object
poly <- rasterToPolygons(rcc, function(x) {x==89}, dissolve = TRUE)
poly <- st_as_sf(poly) 
poly <- st_transform(poly,'+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')

dist <- st_distance(pb.sf, poly, by_element = TRUE)
dist <- as.data.frame(dist)

icepk <- cbind(pb, dist) 
#st_write(poly, "./Tests/icepack_ex.shp") It works!
