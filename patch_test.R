##################################
## TEST PATCH STATS ##############
##################################

rm(list = ls())
library(sp)
library(sf)
library(raster)
library(dplyr)
library(SDMTools)

# create spdf using sp
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(ice.df$X, ice.df$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = ice.df, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

ice <- st_as_sf(pb.spdf.polar) # as sf object 
buf30 <- st_buffer(ice, 30000) # 30 km buffer around points

#---------- Test on a single bear, single point ----------#

Geo <- raster('./SIC-TIFs/MASIE/pb_06817/masie_ice_r00_v01_2006234_4km.tif') # first geotiff in folder
pb <- subset(buf30, animal=='pb_06817' & ymd=='2006-08-22') #single point
pb_sp <- as(pb, 'Spatial')

GeoCrop <- raster::crop(Geo, pb_sp) # Crop raster to bounding box of pb_sp

# test by plotting
plot(GeoCrop, axes=FALSE)
plot(pb_sp, add = TRUE, box = FALSE, axes = FALSE)

GeoCrop_mask <- raster::mask(GeoCrop, pb_sp) #sets cells outside buffer to NA

#test by plotting
plot(GeoCrop_mask, axes = FALSE)
plot(pb_sp, add = TRUE)

# Run ClassStat and PatchStat

ice.pat <- PatchStat(GeoCrop_mask)
ice.pat <- ice.pat[2,] # ice stats only 

pb.df <- as.data.frame(pb_sp) # remove spatial metadata
test <- cbind(pb.df, ice.pat) # add patch stats to existing data
