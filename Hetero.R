######################################################
### LANDSCAPE HETEROGENEITY ##########################
######################################################

# from MASIE data - 4km grid cell size (x=4000m, y=4000m)
# FTP: ftp://sidads.colorado.edu/DATASETS/NOAA/G02186/geotiff/4km/ice_only/

rm(list = ls())

library(dplyr)
library(raster)
library(sf)
library(spData)
library(lubridate)

# create df with start dates
load('all.RData')
start <- subset(all, start.swim==1)

#subset bear
all$ymd <- as.character(ymd(paste(all$year, all$month, all$day))) #get ymd into POSIXct 
all$ymd <- as.POSIXct(all$ymd, tz='US/Alaska')

pb <- subset(all, animal=='pb_06817' & datetime > '2006-08-16' & datetime < '2006-09-17')
pb$ordinal <- yday(pb$ymd) #change ymd to ordinal date
pb$ord.year <- paste("2006", pb$ordinal, sep="")

# create rasterstack using TIFs
rasterlist <- list.files('./SIC-TIFs/MASIE/2006', full.names = TRUE) # bring in all files
rasterlist <- gsub('_', '-', rasterlist) # replace underscore with dash in filename

# separate date component of TIF name to correspond to spdf metadata 

stack<-list()
date<-vector()
for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  tt<-unlist(strsplit(names(stack[[i]]), "[_]"))
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
}

# create spdf
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') # epsg projection 3411 NSIDC sea ice polar stereographic north
coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb, proj4string = projection) 
pb.spdf2 <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

# create null columns for percent ice coverage
pb.spdf2$pct.ice <- NA

# for loop that runs through each point and pulls data from appropriate GeoTIFF
for (i in 1:nrow(pb.spdf2)) {
  st<-stack[[which(date==pb.spdf2$ord.year[i])]]
  pb.spdf2$SIC[i]<-extract(st, pb.spdf2[i,])}



#######################
### From Nathan #######

# calculate percents for forest and ag at .25km2 moving window
pct.forest <- focal(forest.r, w=matrix(1/289, nrow = 17, ncol = 17))
hist(pct.ag)

pct.ag <- focal(ag.r, w=matrix(1/289, nrow = 35, ncol = 35))
hist(pct.ag)

# use a larger focal area for ag at .5km2 moving window
pct.ag.2 <- focal(ag.r, w=matrix(1/1225, nrow = 35, ncol = 35))
hist(pct.ag.2)