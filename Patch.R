################################
### PATCH STATS ################
################################

rm(list = ls())

library(sp)
library(sf)
library(raster)
library(dplyr)
library(SDMTools)

#------------------- PREP DATA ----------------------------#

load('Ice_Measurements.RData')
load('Patch.Rdata')

pb.df <- subset(ice.df, id=='pb_20414.2009')

pb.df <- pb.df %>% 
  filter(!(ord.year=='2009199'))

pb.df <- droplevels(pb.df)

rasterlist <- list.files('./SIC-TIFs/MASIE/pb_20414', full.names = TRUE) # bring in all GeoTIFFs by bear

#---------------- CREATE SPATIAL DATA ---------------------#

# create spdf using sp

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(pb.df$X, pb.df$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb.df, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

track <- st_as_sf(pb.spdf.polar) # as sf object 
plot(st_geometry(track)) # verify track looks good

buf <- st_buffer(track, 30000) # 30 km buffer around points

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

# crop raster to buffer

buf.sp <- as(buf, 'Spatial') # pb as sp object

# pull raster data from GeoTIFF that corresponds to ordinal date

pat <- list()
cs <- list()
for (i in 1:nrow(buf.sp)) {
  st2<-st[[which(date==buf.sp$ord.year[i])]]
  GeoCrop <- raster::crop(st2, buf.sp[i,])
  GeoCrop_mask <- raster::mask(GeoCrop, buf.sp[i,])
  pat[[i]] <- PatchStat(GeoCrop_mask)
  cs[[i]] <- ClassStat(GeoCrop_mask)}
 
pp <- t(sapply(pat, function(i) i[2,])) # if both patchID's present - 0 (not ice) and 3 (ice) 
cs2 <- t(sapply(cs, function(i) i[2,]))



bufpp <- cbind(buf.sp, pp)

bufppcs <- cbind(bufpp, cs2) # new data file for patch stats

patch <- cbind(sdm, bufppcs) # append to previous bears' data 


save(patch, file='Patch.RData')



head(pat)
tail(pat) 


