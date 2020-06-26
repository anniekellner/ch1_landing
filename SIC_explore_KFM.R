########################################################################################
########      EXPLORE SEA ICE CONCENTRATION FOR KNOWN FATE MODEL    ####################
########################################################################################

# McCall et al. 2016: nonlinear association between polar bears and sea ice. 
# SIC + SIC^2 
# see also Durner et al. 2004, 2006, 2009


rm(list = ls())

library(dplyr)
library(raster)

# Load data

load('all_v2.RData')
load('KFM.RData')
load('SIC_KFM.RData')

ids <- unique(bears$id)
pb <- subset(all.v2, id %in% ids)
pb <- subset(pb, year == 2011 & month > 5 & month < 10)

#create rasterstack using TIFs
rasterlist <- list.files(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/OWS_2011", pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=TRUE)

# --------------------------------------------------------------------------------------------------- #

# for loop for creating rasters
# separate date component of TIF name to correspond to spdf metadata 

stack<-list()
date<-vector()
for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  tt<-unlist(strsplit(names(stack[[i]]), '[[:punct:]]+')) #https://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
  values(stack[[i]])[values(stack[[i]])>200]<-0
}

####################################################################
######## create SpatialPointsDataFrame for locations ########

## AMSR-E 1 June 2002 - 4 Oct 2011
## AMSR-2 July 2012 - 17 Nov 2018

#all$ymd <- as.character(ymd(paste(all$year, all$month, all$day))) #get ymd into POSIXct so can be subsetted
#all$ymd <- as.POSIXct(all$ymd, tz='US/Alaska')


projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') # epsg projection 3411 NSIDC sea ice polar stereographic north
coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb, proj4string = projection) 
pb.spdf2 <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic
pb.spdf2$date2 <- format(pb.spdf2$ymd, "%Y%m%d")


# for loop that runs through each point and pulls data from appropriate GeoTIFF

for (i in 1:nrow(pb.spdf2)) {
  st<-stack[[which(date==pb.spdf2$date2[i])]]
  pb.spdf2$SIC_30m_me[i]<-extract(st, pb.spdf2[i,], buffer = 30000, fun = mean, na.rm = TRUE)
  pb.spdf2$SIC_30m_max[i]<-extract(st, pb.spdf2[i,], buffer = 30000, fun = max, na.rm = TRUE)
  pb.spdf2$SIC_30m_min[i]<-extract(st, pb.spdf2[i,], buffer = 30000, fun = min, na.rm = TRUE)
  }



current <- pb.spdf2@data #convert to df
#df <- dplyr::select(df, -date2)

combine <- rbind(combine, current)

head(combine)
tail(combine)

save(combine, file='SIC_KFM.RData')


