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

# separate date component of TIF name to correspond to spdf metadata 

stack <- list()
date<-vector()
for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  tt<-unlist(strsplit(names(stack[[i]]), "[_]"))
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
}

st <- stack(stack)

#substitute 3(ice) with 1 so data is binary
df <- data.frame(id=3, v=1)
st2 <- subs(st, df, subsWithNA=FALSE)

# create spdf
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-crs('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

# create null columns for 10,30,50,100,500 km from bear GPS point
pb.spdf.polar$Buf10_me<- NA
pb.spdf.polar$Buf30_me<- NA
pb.spdf.polar$Buf50_me<- NA
pb.spdf.polar$Buf100_me <- NA
pb.spdf.polar$Buf500_me <- NA


# for loop that runs through each point and pulls data from appropriate GeoTIFF
for (i in 1:nrow(pb.spdf.polar)) {
  st3<-st2[[which(date==pb.spdf.polar$ord.year[i])]] #pulls raster data from GeoTIFF that corresponds to ordinal date
  pb.spdf.polar$Buf10_me[i] <- extract(st3, pb.spdf.polar[i,], buffer=10000, fun=mean, na.rm=T)
  pb.spdf.polar$Buf30_me[i] <- extract(st3, pb.spdf.polar[i,], buffer=30000, fun=mean, na.rm=T)
  pb.spdf.polar$Buf50_me[i] <- extract(st3, pb.spdf.polar[i,], buffer=50000, fun=mean, na.rm=T)
  pb.spdf.polar$Buf100_me[i] <- extract(st3, pb.spdf.polar[i,], buffer=100000, fun=mean, na.rm=T)
  pb.spdf.polar$Buf500_me[i] <- extract(st3, pb.spdf.polar[i,], buffer=500000, fun=mean, na.rm=T)}
  
  
df.pb <- pb.spdf.polar@data #convert to df

# Change pct ice to pct water
df.pb$pct.h20.10 <- 1-df.pb$Buf10_me
df.pb$pct.h20.30 <- 1-df.pb$Buf30_me
df.pb$pct.h20.50 <- 1-df.pb$Buf50_me
df.pb$pct.h20.100 <- 1-df.pb$Buf100_me
df.pb$pct.h20.500 <- 1-df.pb$Buf500_me


save(df.pb, file='Ice_Measurements.RData')

## cell is included if its center is covered by the buffer

## The MASIE products are provided in a polar stereographic projection with the WGS 1984 datum



