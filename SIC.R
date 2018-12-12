rm(list = ls())

library(raster)
library(sp)
library(rgdal)
library(sf)

# SIC Analysis: Departure Dates

#create rasterstack using TIFs
rasterlist <- list.files('./SIC TIFs/Departure_Dates', full.names = TRUE) # bring in all files

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

# change values >200 to 0

#create SpatialPointsDataFrame for start locations
load('all.Rdata')
start <- subset(all, start.swim==1)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(start$X, start$Y)
start.spdf <- SpatialPointsDataFrame(coords = coords, data = start, proj4string = projection) 
start.spdf2 <-spTransform(start.spdf, projection(stack[[1]])) #reproject points to polar stereographic
start.spdf2$date2<-format(start.spdf2$datetime, "%Y%m%d") #POSIXct

# create null columns for mean, min, max SIC
start.spdf2$Buf10_me<-NA
start.spdf2$Buf30_me<-NA
start.spdf2$Buf50_me<-NA
start.spdf2$Buf10_min<-NA
start.spdf2$Buf30_min<-NA
start.spdf2$Buf50_min<-NA
start.spdf2$Buf10_max<-NA
start.spdf2$Buf30_max<-NA
start.spdf2$Buf50_max<-NA

for (i in 1:nrow(start.spdf2)) {
st<-stack[[which(date==start.spdf2$date2[i])]]
start.spdf2$Buf10_me[i]<-extract(st, start.spdf2[i,],buffer=10000, fun=mean, na.rm=T)
start.spdf2$Buf10_max[i]<-extract(st, start.spdf2[i,],buffer=10000, fun=max, na.rm=T)
start.spdf2$Buf10_min[i]<-extract(st, start.spdf2[i,],buffer=10000, fun=min, na.rm=T)
start.spdf2$Buf30_me[i]<-extract(st, start.spdf2[i,],buffer=30000, fun=mean, na.rm=T)
start.spdf2$Buf30_max[i]<-extract(st, start.spdf2[i,],buffer=30000, fun=max, na.rm=T)
start.spdf2$Buf30_min[i]<-extract(st, start.spdf2[i,],buffer=30000, fun=min, na.rm=T)
start.spdf2$Buf50_me[i]<-extract(st, start.spdf2[i,],buffer=50000, fun=mean, na.rm=T)
start.spdf2$Buf50_max[i]<-extract(st, start.spdf2[i,],buffer=50000, fun=max, na.rm=T)
start.spdf2$Buf50_min[i]<-extract(st, start.spdf2[i,],buffer=50000, fun=min, na.rm=T)}

df <- start.spdf2@data #convert to df
save(start.spdf2, file='SIC_spdf.RData') #save as spdf
save(df, file='SIC_df.RData')
##########################################################################################################################
#create buffers 50,30,10 km ### THIS STEP NOT NECESSARY ####
buff50k <- buffer(start.spdf, width=50000, dissolve=FALSE) #verified through plot(buff50k)
buff30k <- buffer(start.spdf, width=30000, dissolve=FALSE)
buff10k <- buffer(start.spdf, width=10000, dissolve=FALSE)