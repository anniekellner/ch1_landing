rm(list = ls())

########################
### TROUBLESHOOTING ####
#### 4/10/19 ###########

## Comparing all_loc_ymd shapefile to .RData files to see why X and Y are flipped in newer plots

library(sf)
library(lubridate)

all.shp <- st_read('./Shapefiles/all_loc_ymd.shp', 'all_loc_ymd')

all.shp$date_ymd <- ymd(all.shp$date_ymd)

pb <- subset(all.shp, animal=='pb_06817' & date_ymd > '2006-08-22' & date_ymd < '2006-09-22')

plot(st_geometry(pb)) # this plot is right-side up

################################
## TEST LAT/LONG ##############


load('all.RData')

latlong <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') 

coords <- cbind(all$gps_lon, all$gps_lat)
all.spdf <- SpatialPointsDataFrame(coords=coords, data=all, proj4string = latlong)

all.sf <- st_as_sf(all.spdf)

pb2 <- subset(all.sf, animal=='pb_06817' & ymd > '2006-08-22' & ymd < '2006-09-22')

plot(st_geometry(pb2)) # good

####################################
## TEST POLAR STEREOGRAPHIC ########

polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
all.stereo <- spTransform(all.spdf, polar.stereo) #reproject points to polar stereographic

stereo.sf <- st_as_sf(all.stereo)

pb3 <- subset(stereo.sf, animal=='pb_06817' & ymd > '2006-08-22' & ymd < '2006-09-22')

plot(st_geometry(pb3)) # Polar stereographic is what flips the points

######################################
## SEE HOW MASIE DATA PRESENTS #######

library(raster)

masie <- raster('./SIC-TIFs/MASIE/pb_06817/masie_ice_r00_v01_2006234_4km.tif')
plot(masie)

### ALL GOOD! ######################
#####################################

## JUNK ############################

rm(list = ls())

load('Ice_Measurements.RData')
load('all_v2.RData')
load('all.RData')

ice.t <- subset(ice.df, animal=='pb_06817' & datetime > '2006-08-22' & datetime < '2006-09-22')
all.v2.t <- subset(all.v2, animal=='pb_06817' & datetime > '2006-08-22' & datetime < '2006-09-22')
all.t <- subset(all, animal=='pb_06817' & datetime > '2006-08-22' & datetime < '2006-09-22')

library(sp)

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(ice.t$X, ice.t$Y)
ice.spdf <- SpatialPointsDataFrame(coords = coords, data = ice.t, proj4string = projection) 
ice.polar <-spTransform(ice.spdf, polar.stereo) 

ice.sf <- st_as_sf(ice.polar)
plot(st_geometry(ice.sf))

coords <- cbind(all.v2.t$X, all.v2.t$Y)
allv2.spdf <- SpatialPointsDataFrame(coords = coords, data = all.v2.t, proj4string = projection) 
allv2.polar <-spTransform(allv2.spdf, polar.stereo) 

allv2.sf <- st_as_sf(allv2.polar)
plot(st_geometry(allv2.sf))

coords <- cbind(all.t$X, all.t$Y)
all.spdf <- SpatialPointsDataFrame(coords = coords, data = all.t, proj4string = projection) 
all.polar <-spTransform(all.spdf, polar.stereo) 

all.sf <- st_as_sf(all.polar)
plot(st_geometry(all.sf))

                