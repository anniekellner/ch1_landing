################################
### PATCH STATS ################
################################

# MASIE FTP: sidads.colorado.edu Directory: /pub/DATASETS/NOAA/G02186/geotiff/4km
# username: anonymous
# pw: annie.kellner@colostate.edu


rm(list = ls())

library(sp)
library(sf)
library(raster)
library(dplyr)
library(SDMTools)
library(landscapemetrics)

#is2009 = TRUE

#------------------- PREP DATA ----------------------------#

load('ded_ids.RData')
load('Ice_Measurements.RData')
load('Patch.Rdata')

pb.df <- subset(ice.df, id=='pb_20446.2009')

if(is2009){
pb.df <- pb.df %>% 
  filter(!(ord.year=='2009181'))} # date is missing from MASIE data

pb.df <- droplevels(pb.df)

rasterlist <- list.files('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFsMASIEpb_20446', full.names = TRUE) # bring in all GeoTIFFs by bear

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

cs <- list()
for (i in 1:nrow(buf.sp)) {
  st2<-st[[which(date==buf.sp$ord.year[i])]]
  GeoCrop <- raster::crop(st2, buf.sp[i,])
  GeoCrop_mask <- raster::mask(GeoCrop, buf.sp[i,])
  cs[[i]] <- ClassStat(GeoCrop_mask)}
 

# add index number to list elements

for (i in 1:length(cs)){
  cs[[i]][["Index"]] <- i
}

# ----------------------------------------------------------------------------------------------------------------------------- #
# WORKING SCRIPT - TRIAL AND ERROR #

# Create dataframes out of list elements so can join together later

only3 <- lapply(cs, function(x) cs[[x]]$class == 3)
only3 <- water[water[[1]]$class == 3]

water <- cs[sapply(cs, function(x) x[[1]][[1]]==0)] 


water.only <- water[sapply, function(x) [[1]]]

p2.df <- data.frame(matrix(unlist(p2), nrow = nrow(water), byrow = F)) # this works to convert list to dataframe

water <- t(sapply(water, function(i) i[2,])) # keep only class = 3
water.df <- data.frame(matrix(unlist(water), nrow = nrow(water), byrow = F)) # this works to convert list to dataframe
colnames(water.df)<- colnames(water)  # add back column names

ice <- cs[sapply(cs, function(x) x[[1]][[1]]==3)] 
ice <- t(sapply(ice, function(i) i[1,])) 
ice.df <- data.frame(matrix(unlist(ice), nrow = nrow(ice), byrow = F)) # this works to convert list to dataframe
colnames(ice.df)<- colnames(ice)


cs2 <- rbind(water.df, ice.df) # combine data for class = 3 only  


# Figure out how to convert cs2 to dataframe so can arrange by Index

cs3 <- arrange(cs2, Index) # does not work
  


patch2 <- cbind(buf.sp, cs2)
patch2 <- st_as_sf(patch2)

patch <- rbind(patch, patch2)

save(patch, file='Patch.RData')

# ------------------------------------------------------------------------------------------------------------------------- #

tail(test)              
