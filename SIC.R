rm(list = ls()) # clear environment

#load packages
library(raster)
library(sp)
library(rgdal)
library(sf)
library(lubridate)

# SIC Analysis: Departure Dates

#create rasterstack using TIFs
rasterlist <- list.files('./SIC-TIFs/pb_06817', full.names = TRUE) # bring in all files

# for loop for creating rasters
# separate date component of TIF name to correspond to spdf metadata 

stack<-list()
date<-vector()
for (i in 1:length(rasterlist)) {
stack[[i]]<-raster(rasterlist[i])
tt<-unlist(strsplit(names(stack[[i]]), "[.]"))
date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
values(stack[[i]])[values(stack[[i]])>200]<-0
}

# change values >200 to 0 (values > 200 = land)

####################################################################
######## create SpatialPointsDataFrame for locations ########

## AMSR-E 1 June 2002 - 4 Oct 2011
## AMSR-2 July 2012 - 17 Nov 2018

load('all.Rdata')

all$ymd <- as.character(ymd(paste(all$year, all$month, all$day))) #get ymd into POSIXct so can be subsetted
all$ymd <- as.POSIXct(all$ymd, tz='US/Alaska')

pb <- subset(all, id=='pb_06817.2006' & ymd > '2006-08-21' & ymd <= '2006-09-21') # subsetting from master data file ('all.Rdata')

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') # epsg projection 3411 NSIDC sea ice polar stereographic north
coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb, proj4string = projection) 
pb.spdf2 <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic
pb.spdf2$date2 <- format(pb.spdf2$ymd, "%Y%m%d")

# create null columns for SIC
pb.spdf2$SIC <- NA

# for loop that runs through each point and pulls data from appropriate GeoTIFF
for (i in 1:nrow(pb.spdf2)) {
st<-stack[[which(date==pb.spdf2$date2[i])]]
pb.spdf2$SIC[i]<-extract(st, pb.spdf2[i,])}

#start.spdf2$Buf10_max[i]<-extract(st, start.spdf2[i,],buffer=10000, fun=max, na.rm=T)


df <- pb.spdf2@data #convert to df
save(start.spdf2, file='SIC_spdf.RData') #save as spdf
save(df, file='SIC_df.RData')
