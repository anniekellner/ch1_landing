#################################################
###   DISTANCE TO THE ICE PACK    ###############
#################################################


rm(list = ls())

library(raster)
library(sp)
library(sf)
library(rgdal)

# load data

load('ice_calc.RData') #GPS data
filelist <- dir(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/POLY/pb_06817.2006", pattern='.shp', all.files=TRUE, recursive = TRUE, full.names=TRUE)
pb <- subset(ice.calc, animal=='pb_06817') # test with one bear

# Create spatial object. This works - always do it this way!

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') # matches AMSRE raster
coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=pb, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo)
pb.spdf.polar$date2 <- format(pb.spdf.polar$ymd, "%Y%m%d") # format for matching up with dates on GeoTIFFs

track <- st_as_sf(pb.spdf.polar) # as sf object 
plot(st_geometry(track))  # verify track looks good


# Test projection compatibility using one shapefile

shp <- st_read(filelist[1])
plot(st_geometry(shp))
plot(st_geometry(track), add = TRUE)

# Make sure distance calculation works

disttest <- st_distance(track[1,], shp) # works!
# if crs(x) doesn't equal crs(y):
st_crs(shp) <- st_crs(track)
shp <- st_transform(shp, polar.stereo)


# ------------------------------------------------------------------------------------------------

# Associate GPS point with polygon file name

dist <- vector()
for (i in 1:nrow(track)){
  file <- filelist[grep(track$date2[i], filelist)]
  shp <- st_read(file)
  st_transform(shp, polar.stereo)
  dist[i] <- st_distance(track[i], shp, by_element = FALSE)}











