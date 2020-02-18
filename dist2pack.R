##############################################################
## DISTANCE TO 15% SIC #######################################
##############################################################

rm(list = ls())

library(raster)
library(sp)
library(sf)

# ------------ Data --------------------------------#

load('ice_calc.RData') #GPS data
pb <- subset(ice.calc, animal=='pb_06817')


rasterlist <- list.files('C:/Users/akell/Documents/ArcGIS/Polar_Bears_GIS/R/Chapter1/Landing Analysis/ch1_landing/SIC-TIFs/SIC_univ_Bremen/RCC/pb_06817.2006', full.names = TRUE) # bring in all GeoTIFFs by bear

# Create spatial object

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') # matches AMSRE raster
coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=pb, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo)
pb.spdf.polar$date2 <- format(pb.spdf.polar$ymd, "%Y%m%d") # format for matching up with dates on GeoTIFFs

track <- st_as_sf(pb.spdf.polar) # as sf object 
plot(st_geometry(track)) # verify track looks good

# separate date component of TIF name to correspond to spdf metadata 

stack <- list()
date<-vector()
for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  tt<-unlist(strsplit(names(stack[[i]]), "[.]"))
  date[i] <-tt[which(nchar(tt)==max(nchar(tt)))]
}



# --------- Find Distance ------------------------ #


# for loop that runs through each point and pulls data from appropriate GeoTIFF

dist <- vector()
for (i in 1:nrow(track)) {
  j <- which(track$date2[i]==date)
  r <- stack[[j]] #single raster that corresponds to date
  gv <- getValues(r) # change values to vector so can get mode
  mode <- modal(gv, na.rm=TRUE) # find mode
  poly <- rasterToPolygons(r, function(x) {x==mode}, dissolve = TRUE) # convert to polygon
  poly2 <- st_as_sf(poly) #sf object
  dist <- st_distance(track[i], poly2, by_element = FALSE)}
  
dist <- as.data.frame(dist)

pb <- cbind(pb, dist)

# ----------- TROUBLESHOOTING ------------------------ #

# Problem: distance from first points to first raster is 0.00, but in ArcGIS does not appear to be 0.00. Plot in sf to see whether issue is projections.

plot(st_geometry(track))

r <- raster(rasterlist[[1]])
gv <- getValues(r) # change values to vector so can get mode
mode <- modal(gv, na.rm=TRUE)
poly <- rasterToPolygons(r, function(x) {x==mode}, dissolve = TRUE) # convert to polygon
poly2 <- st_as_sf(poly)

plot(st_geometry(poly2))
plot(st_geometry(track))

# helpful for plotting sf objects: https://r-spatial.github.io/sf/reference/plot.html

# try using ggplot2

library(ggplot2)
ggplot() + geom_sf(data = poly2) + geom_sf(data = test)

library(dplyr)
test <- filter(track, date2 == '20060821')

# Results from using single raster
#           dist
#1 14724.83 [m]
#2 15695.41 [m]
#3 15708.00 [m]