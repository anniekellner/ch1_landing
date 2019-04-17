############################################
##        DISTANCE TO PACK ICE     #########
############################################

rm(list = ls())

library(raster)
library(sp)
library(rgeos)
library(sf)
library(dplyr)
library(tidyr)

# --------------------------- load data ---------------------------------------------#

rasterlist <- list.files('./SIC-TIFs/SIC_univ_Bremen/RCC/pb_06817.2006', full.names = TRUE) # bring in all files

load('ice_calc.RData')
pb <- subset(ice.calc, id=='pb_06817.2006')


# separate date component of TIF name to correspond to spdf metadata 

pb$date2 <- format(pb$ymd, "%Y%m%d")

stack<-list()
date<-vector()
for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  tt<-unlist(strsplit(names(stack[[i]]), '[[:punct:]]+')) #https://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
  values(stack[[i]])[values(stack[[i]])>200]<-0
}

st <- stack(stack)

# ------------------------- create spatial objects ------------------------------------#

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster

# points

coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) 
pb.sf <- st_as_sf(pb.spdf.polar) #convert to sf object

# for loop that runs through each point and pulls data from appropriate GeoTIFF

for (i in 1:nrow(pb.sf)) {
  ras <- st[[which(date==pb.sf$date2[i])]] # select raster that corresponds
  gv <- getValues(ras) # change values to vector so can get mode
  mode <- modal(gv, na.rm=TRUE) # find mode
  poly <- rasterToPolygons(ras, function(x) {x==mode}, dissolve = TRUE)
  poly <- st_as_sf(poly)
  poly <- st_transform(poly, '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')
  dist <- st_distance(pb.sf, poly, by_element = TRUE)
  }

dist <- as.data.frame(dist)

pkice <- cbind(pb, dist) 

save(pkice, 'pkice_timedist.RData')

#####################################################################################

#This works but takes too long to convert raster to polygon every time. Do same thing but use polygons (.shp)

unlist(strsplit(colnames(poly),'[[:punct:]]+')) # YES!!!! THIS WILL WORK!!!!!






