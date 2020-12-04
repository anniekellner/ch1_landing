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

setwd("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/start_swim/rasters")

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

points2 <- points %>%
  select(id, datetime, dist2pack) %>%
  arrange(dist2pack)

mean(points2$dist2pack)

# ---- MAP ----------------------------------------------------------------- #

# pb_20446 is minimum distance from pack ice
# pb_20945 is max

# Find rasters with min and max extent

filelist <- dir(path = "./RCC", pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=TRUE)

pack <- list()

for (i in 1:length(filelist)) {
    r <- raster(filelist[i]) #read in raster
    gv <- getValues(r) # change values to vector so can get mode
    mode <- modal(gv, na.rm=TRUE) # find mode
    new <- calc(r, fun=function(x){x==mode})
    pack[[i]] <- new
  }
  
size <- vector()

for(i in 1:length(pack)){
  size[i] <- cellStats(pack[[i]], 'sum')
}

# minimum ice extent is 2012
# maximum is 2009 (07/18/2009)



plot(avg_ice)


min_shp <- st_read("asi-n3125-20110728-v5.4.tif.shp")
max_shp <- st_read("asi-AMSR2-n3125-20150827-v5.4.shp")

min_pt <- filter(points, id == "pb_20446.2009")
max_pt <- filter(points, id == "pb_20845.2015")

min <- tm_shape(min_shp) + 
  tm_borders() + 
  tm_shape(min_pt) + 
  tm_dots()

