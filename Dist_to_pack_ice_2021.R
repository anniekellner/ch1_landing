##################################
###     DISTANCE TO PACK 2021   ##
##################################

rm(list = ls())

library(raster)
library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(lubridate)

# Load Data

load('./data/RData/land_bears_CoxPH.RData') #GPS data
bears <- full

bears <- distinct(bears)

filelist <- dir(path = "D:/Polar Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/POLY", pattern='.shp', all.files=TRUE, recursive = TRUE, full.names=TRUE)

# Create spatial object

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(bears$X, bears$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=bears, proj4string = projection) 

# Create sf object and project to MASIE CRS

shp <- st_read(filelist[1])
st_crs(shp)

sf <- st_as_sf(pb.spdf)
sf <- st_transform(sf, st_crs(shp))

# Visualize data to make sure projections line up

plot(st_geometry(shp))
plot(st_geometry(sf), add = TRUE)

# Create column to match with MASIE raster
sf$date2 <- gsub("-", "", bears$ymd) # format for matching up with dates on GeoTIFFs

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

# Associate GPS point with polygon file name

dist <- vector()
for (i in 1:nrow(sf)){
  file <- filelist[grep(sf$date2[i], filelist)]
  shp <- st_read(file)
  sf <- st_transform(sf, st_crs(shp))
  dist[i] <- st_distance(sf[i,], shp)
}

save(dist, file = './data/RData/dist1.Rds')

sf$dist2pack <- dist
