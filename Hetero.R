######################################################
### LANDSCAPE HETEROGENEITY ##########################
######################################################

# from MASIE data - 4km grid cell size (x=4000m, y=4000m)
# FTP: ftp://sidads.colorado.edu/DATASETS/NOAA/G02186/geotiff/4km/ice_only/
### FTP issues!!!! FTP does not give access to the right files 3/23/19 ####

rm(list = ls())

library(dplyr)
library(raster)
library(sf)
library(spData)
library(lubridate)

# create df with start dates
load('all_v2.RData')
#start <- subset(all, start.swim==1)

#subset bear
#all$ymd <- as.character(ymd(paste(all$year, all$month, all$day))) #get ymd into POSIXct 
#all$ymd <- as.POSIXct(all$ymd, tz='US/Alaska')

pb <- subset(all.v2, animal=='pb_06817' & datetime > '2006-08-22' & datetime < '2006-09-21')
pb$ordinal <- yday(pb$ymd) #change ymd to ordinal date
pb$ord.year <- paste("2006", pb$ordinal, sep="")

# create rasterstack using TIFs
rasterlist <- list.files('./SIC-TIFs/MASIE/pb_06817', full.names = TRUE) # bring in all files
st <- stack(rasterlist)
df <- data.frame(id=3, v=1)
st2 <- subs(st, df, subsWithNA=FALSE)

# separate date component of TIF name to correspond to spdf metadata 

stack<-list()
date<-vector()
for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  tt<-unlist(strsplit(names(stack[[i]]), "[_]"))
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
}

  



  
## Want to substitute to make raster binary but am lost ###
#stack2 <- subs(stack, data.frame(id=c(1,3), v=c(0,1)), by='Color Index')
#y <- subs(x, data.frame(id=c(2,3), v=c(40,50)))
# create spdf
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') # epsg projection 3411 NSIDC sea ice polar stereographic north
coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

# create null columns for percent ice coverage
pb.spdf.polar$pct.ice <- NA

# for loop that runs through each point and pulls data from appropriate GeoTIFF
for (i in 1:nrow(pb.spdf.polar)) {
  st<-stack[[which(date==pb.spdf.polar$ord.year[i])]] #pulls raster data from GeoTIFF that corresponds to ordinal date
  pb.spdf.polar$pct.ice[i]<-extract(st, pb.spdf.polar[i,])}


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