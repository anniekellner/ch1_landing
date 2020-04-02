######################################################
### LANDSCAPE HETEROGENEITY ##########################
######################################################

# from MASIE data - 4km grid cell size (x=4000m, y=4000m)
# FTP: ftp://sidads.colorado.edu/DATASETS/NOAA/G02186/geotiff/4km/ice_only/
### FTP issues!!!! FTP does not give access to the right files 3/23/19 ####

rm(list = ls())

library(dplyr)
library(raster)
library(spData)
library(lubridate)

# create df with start dates

load('all_v2.RData')
load('Ice_Measurements.RData')

start <- subset(all.v2, start.swim==1)
start <- start %>% # remove 'undecided'
  filter(!(id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004'))
start <- droplevels(start)

#subset bear
all.v2$ymd <- as.character(ymd(paste(all.v2$year, all.v2$month, all.v2$day))) #get ymd into POSIXct 
all.v2$ymd <- as.POSIXct(all.v2$ymd, tz='US/Alaska')

pb <- subset(all.v2, animal=='pb_20446' & datetime >= '2009-06-28' & datetime < '2009-07-29') 
pb$ordinal <- yday(pb$ymd) #change ymd to ordinal date
pb$ord.year <- paste("2009", pb$ordinal, sep="")
pb <- head(pb, -4) # remove rows after bear has started swimming

#pb <- pb %>% #remove 2009181 because no data in MASIE
  #filter(!(ord.year == '2009181'))
#pb <- droplevels(pb)

# create rasterstack using TIFs
rasterlist <- list.files('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/MASIE/pb_20446', full.names = TRUE) # bring in all files

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

df.new <- pb.spdf.polar@data #convert to df

# Change pct ice to pct water
df.new$pct.h20.10 <- NA
df.new$pct.h20.30 <- NA
df.new$pct.h20.50 <- NA
df.new$pct.h20.100 <- NA
df.new$pct.h20.500 <- NA

# Add missing data to dataframe

b4 <- subset(ice.df, animal=="pb_20446") # 7 missing rows

missing <- df.new %>% slice(tail(row_number(), 7)) # select last row from pb dataframe (missing data 4/1/2020)

ice.df <- rbind(ice.df, missing)
save(ice.df, file='Ice_Measurements.RData')

## cell is included if its center is covered by the buffer

## The MASIE products are provided in a polar stereographic projection with the WGS 1984 datum





