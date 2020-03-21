############################################################################
#######     LANDSCAPE METRICS USING PACKAGE LSM       ######################
############################################################################

library(sp)
library(sf)
library(raster)
library(dplyr)
library(landscapemetrics)

load('Patch.RData') # check new package against what is already done
load('Ice_Measurements.RData')
pb.df <- subset(ice.df, id=='pb_20446.2009')

rasterlist <- list.files('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/MASIE/pb_20446', full.names = TRUE) # bring in all GeoTIFFs by bear

#---------------- CREATE SPATIAL DATA ---------------------#

# create spdf using sp

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(pb.df$X, pb.df$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb.df, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

#------------ANALYSIS-----------------------------#

# separate date component of TIF name to correspond to spdf metadata 

stack <- list()
date<-vector()
for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  tt<-unlist(strsplit(names(stack[[i]]), "[_]"))
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
}

st <- stack(stack)

check_landscape(st[[1]]) # rasters look good
list_lsm(level = "class") # to see all class level metrics

# Try using lsm 
test <- scale_sample(st[[1]], pb.spdf.polar[1:2,],  size = 10000, max_size = 50000, level = "class") # this works!

cs <- list()
for (i in 1:nrow(pb.spdf.polar)) {
  st2<-st[[which(date==pb.spdf.polar$ord.year[i])]]
  cs[[i]] <- scale_sample(st2, pb.spdf.polar[i], size = 10000, max_size = 50000, level = "class" )
}
  
  

   
 
  
 

