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

                