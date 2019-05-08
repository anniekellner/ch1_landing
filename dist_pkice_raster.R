##########################################################
##        DISTANCE TO PACK ICE - USING RASTER    #########
##########################################################

rm(list = ls())

library(raster)

# load data
r <- raster('./SIC-TIFs/SIC_univ_Bremen/RCC/pb_06817.2006/asi-n6250-20060821-v5.4.tif')
load('ice_calc.RData')
pb <- subset(ice.calc, id=='pb_06817.2006')

# create spatial objects (pts)

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) 


# reclass raster with all NA except mode

v <- getValues(r)
mode <- modal(v, na.rm=TRUE)

rc.mode <- function(x){
  ifelse(x==mode,1,NA)
}

new.ras <- overlay(r, fun=rc.mode)


# distance function

dist.r <- raster::distance(new.ras) #create distance raster
crs(dist.r) <- '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0'

points <- SpatialPoints(cbind(pb$X, pb$Y), proj4string = polar.stereo) # Sp object for extract function   

test <- raster::extract(dist.r, points)




# extract function

c(1,15,0, 15,100,1, 100,255,0)

for (i in 1:nrow(pb.sf)) {
  ras <- st[[which(date==pb.sf$date2[i])]] # select raster that corresponds
  gv <- getValues(ras) # change values to vector so can get mode
  mode <- modal(gv, na.rm=TRUE) # find mode
  poly <- rasterToPolygons(ras, function(x) {x==mode}, dissolve = TRUE)
  poly <- st_as_sf(poly)
  poly <- st_transform(poly, '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')
  dist <- st_distance(pb.sf, poly, by_element = TRUE)
}



pb.sf <- st_as_sf(pb.spdf.polar) #convert to sf object
poly <- rasterToPolygons(rcc, function(x) {x==89}, dissolve = TRUE)
poly <- st_as_sf(poly) 
poly <- st_transform(poly,'+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')

dist <- st_distance(pb.sf, poly, by_element = TRUE)

