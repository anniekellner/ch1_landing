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
polar.stereo <-'+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs' # matches AMSRE raster

track <-st_as_sf(pb, coords = c("gps_lon", "gps_lat"), crs = polar.stereo, agr = 'constant')
plot(st_geometry(track)) # verify track looks good

# ------------------------------------------------------------------------------------------------

# create list of shapefiles 

sptest <- readOGR(filelist[[1]]) # test: sp seems to preserve file info better than sf
sftest <- st_read(filelist[[1]])
rastershp <- shapefile(filelist[[1]])

shp.list <- list()
for (i in 1:length(filelist)){
  shp.list[[i]] <- shapefile(filelist[i])
}

# separate date component of POLY name to correspond to spdf metadata 

sptest

# find out where name/date is in shp metadata
shptest$geometry$

shp.list <- list()
date<-vector()
for (i in 1:length(filelist)) {
  shp.list[[i]]<-st_read(filelist[i])
  tt<-unlist(strsplit(names(stack[[i]]), "[.]"))
  date[i] <-tt[which(nchar(tt)==max(nchar(tt)))]
}