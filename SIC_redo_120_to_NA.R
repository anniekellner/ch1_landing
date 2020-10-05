rm(list = ls())

library(sf)
library(raster)
library(dplyr)
library(tmap)
library(lubridate)

load("all_v2.RData")
load("SIC_new.RData")

lb <- subset(all.v2, land_bear == 1)

ss <- subset(lb, start.swim == 1) # 18 swims after adding data below
ss <- unique(ss$id)

bears <- subset(lb, lb$id %in% ss)
bears <- filter(bears, month > 5 & month < 10)

sub <- subset(bears, year == 2008)


#---------------- SPATIAL DATA ---------------------#

rasterlist <- list.files('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/OWS_2008', pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=TRUE)


# create spdf using sp

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +x_0=0 +y_0=0 +a=6378273 +rf=298.279411123064 +units=m +no_defs') # matches AMSRE raster
coords <- cbind(sub$X, sub$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = sub, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

pb.spdf.polar$ymd <- ymd(pb.spdf.polar$ymd)
pb.spdf.polar$date2 <- format(pb.spdf.polar$ymd, "%Y%m%d")


# separate date component of TIF name to correspond to spdf metadata 
# Data@names has a "." instead of a "_" in the names slot

stack<-list()
date<-vector()

for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  stack[[i]][stack[[i]] == 120] <- NA # change land from 120 to NA in v5.4
  tt<-unlist(strsplit(names(stack[[i]]), '[[:punct:]]+')) #https://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
}

st <- stack(stack)

# for loop that runs through each point and pulls data from appropriate GeoTIFF

pb.spdf.polar$SIC_30m_me <- NULL
pb.spdf.polar$SIC_30m_max <- NULL
pb.spdf.polar$SIC_30m_min <- NULL


for (i in 1:nrow(pb.spdf.polar)) {
  st2<-st[[which(date==pb.spdf.polar$date2[i])]]
  pb.spdf.polar$SIC_30m_me[i]<-extract(st2, pb.spdf.polar[i,], buffer = 30000, fun = mean, na.rm = TRUE)
  pb.spdf.polar$SIC_30m_max[i]<-extract(st2, pb.spdf.polar[i,], buffer = 30000, fun = max, na.rm = TRUE)
  pb.spdf.polar$SIC_30m_min[i]<-extract(st2, pb.spdf.polar[i,], buffer = 30000, fun = min, na.rm = TRUE)
}


new <- pb.spdf.polar@data # bears just run

head(new)
tail(new)

all_SIC_new <- rbind(all_SIC_new, new) # all bears

save(all_SIC_new, file = "SIC_new.RData")

