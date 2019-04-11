################################
### PATCH STATS ################
################################

rm(list = ls())

library(sp)
library(sf)
library(raster)
library(plyr)
library(SDMTools)

#------------------- PREP DATA ----------------------------#

load('Ice_Measurements.RData')

# create spdf using sp
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(ice.df$X, ice.df$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = ice.df, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

ice <- st_as_sf(pb.spdf.polar) # as sf object 
buf30 <- st_buffer(ice, 30000) # 30 km buffer around points

pb <- subset(buf30, id=='pb_06817.2006')


rasterlist <- list.files('./SIC-TIFs/MASIE/pb_06817', full.names = TRUE) # bring in all GeoTIFFs by bear

# separate date component of TIF name to correspond to spdf metadata 

stack <- list()
date<-vector()
for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  tt<-unlist(strsplit(names(stack[[i]]), "[_]"))
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
}

st <- stack(stack)

# crop raster to buffer

pb.sp <- as(pb, 'Spatial') # pb as sp object

# pull raster data from GeoTIFF that corresponds to ordinal date

ice.pat <- list()
for (i in 1:nrow(pb.sp)) {
  st2<-st[[which(date==pb.sp$ord.year[i])]]
  GeoCrop <- raster::crop(st2, pb.sp[i,])
  GeoCrop_mask <- raster::mask(GeoCrop, pb.sp[i,])
  ice.pat[[i]] <- PatchStat(GeoCrop_mask)}
  
ice.pat2 <- lapply(ice.pat, `[`, -1,) # remove first row in each list element

pb <- as.data.frame(pb)
test10 <- merge(pb,df.test2) 

 
df.test <- data.frame(matrix(unlist(ice.pat2), nrow=length(ice.pat2), byrow=T),stringsAsFactors=FALSE)
  

df.test2 <- ldply (ice.pat2, data.frame) 

 


