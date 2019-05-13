##############################################################
## DISTANCE TO 15% SIC: ONE BEAR, ONE RASTER #################
##############################################################

# Calculate bear's distance to pack ice ###
# Reclassify all ice >=15% with a single value
# use clump function (directions = 8) to identify clumps

rm(list = ls())

library(raster)
library(sp)
library(sf)

# load data

r <- raster('./SIC-TIFs/SIC_univ_Bremen/Orig_decided/pb_06817.2006/asi-n6250-20060821-v5.4.tif')
load('ice_calc.RData')
pb <- subset(ice.calc, animal=='pb_06817')


########### 
# Create spatial object

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') # matches AMSRE raster
coords <- cbind(pb$X, pb$Y)
pb.sp <- SpatialPoints(coords = coords, proj4string = projection) 
pb.sp.polar <-spTransform(pb.sp, polar.stereo) 

#############
# Prep raster

# Reclassify raster so SIC>15 = 1 and all else = 0
rc <- reclassify(r, c(1,15,0, 15,100,1, 100,255,0))

# Clump

rcc <- clump(rc, directions=8) # all cells adjacent to main pack ice with 15% SIC or greater are considered part of ice pack
plot(rcc)

#################
#### Distance ###

gv <- getValues(rcc) # change values to vector so can get mode
mode <- modal(gv, na.rm=TRUE) # find mode

pb.sf <- st_as_sf(pb.spdf.polar) #convert to sf object
poly <- rasterToPolygons(rcc, function(x) {x==mode}, dissolve = TRUE) # convert to polygon
poly <- st_as_sf(poly) #sf object 
dist <- st_distance(pb.sf, poly, by_element = TRUE)
dist <- as.data.frame(dist)

icepk <- cbind(pb, dist) 
#st_write(poly, "./Tests/icepack_ex.shp") It works!  