######################################################################
###   DISTANCE TO THE ICE PACK FROM DEPARTURE POINT   ###############
######################################################################


rm(list = ls())

library(raster)
library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(lubridate)

# load data

load('coxph.RData') #GPS data
swim <- subset(bears, start.swim == 1)
swim <- distinct(swim)
filelist <- dir(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/start_swim/shapefiles", pattern='.shp', all.files=TRUE, recursive = TRUE, full.names=TRUE)
#pb <- subset(swim, id=='pb_06817.2006') # test with one bear

# Create spatial object. This works - always do it this way!

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') # matches AMSRE raster
coords <- cbind(swim$X, swim$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=swim, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo)
pb.spdf.polar$date2 <- gsub("-", "", pb.spdf.polar$ymd) # format for matching up with dates on GeoTIFFs
swim.sf <- st_as_sf(pb.spdf.polar)

#### CHECK WITH ONE BEAR TO SEE WHETHER PROCESS LOOKS GOOD #######

#track <- st_as_sf(pb.spdf.polar) # as sf object 
#plot(st_geometry(track))  # verify track looks good


# Test projection compatibility using one shapefile

shp <- st_read(filelist[2])
points <- st_transform(swim.sf, st_crs(shp)) # transform track to shp projection

plot(st_geometry(shp))
plot(st_geometry(points), add = TRUE)

library(tmap)

tmap_mode("view")

tm_shape(shp) +
  tm_borders() + 
  tm_shape(points) + 
  tm_dots()

# looks good on map

# Make sure distance calculation works

#disttest <- st_distance(track, shp) # works!

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

# Associate GPS point with polygon file name

dist <- vector()
for (i in 1:nrow(points)){
  file <- filelist[grep(points$date2[i], filelist)]
  shp <- st_read(file)
  points <- st_transform(points, st_crs(shp))
  dist[i] <- st_distance(points[i,], shp)
  }

points$dist2pack <- dist

# ----- STATS ------------------------------------------------------------- #

mean(points$dist2pack)



