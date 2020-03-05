#################################################
###   DISTANCE TO THE ICE PACK    ###############
#################################################


rm(list = ls())

library(raster)
library(sp)
library(sf)
library(rgdal)

# load data

load('ice_calc.RData') #GPS data
filelist <- dir(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/POLY/pb_06817.2006", pattern='.shp', all.files=TRUE, recursive = TRUE, full.names=TRUE)
pb <- subset(ice.calc, animal=='pb_06817') # test with one bear

# Create spatial object. This works - always do it this way!

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') # matches AMSRE raster
coords <- cbind(pb$X, pb$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=pb, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo)
pb.spdf.polar$date2 <- format(pb.spdf.polar$ymd, "%Y%m%d") # format for matching up with dates on GeoTIFFs

track <- st_as_sf(pb.spdf.polar) # as sf object 
plot(st_geometry(track)) 

plot(st_geometry(track)) # verify track looks good

# ------------------------------------------------------------------------------------------------

# Associate GPS point with polygon file name

track$date2 <- format(track$ymd, "%Y%m%d")

dist <- vector()
for (i in 1:nrow(track)){
  file <- filelist[grep(track$date2[i], filelist)]
  shp <- st_read(file)
  st_transform(shp, polar.stereo)
  dist[i] <- st_distance(track[i], shp, by_element = FALSE)}

disttest <- st_distance(track[1,], shp) # works!

# projection - see what raster data is in
ras <- raster('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/RCC/pb_06817.2006/asi-n6250-20060821-v5.4.tif')

track <- st_transform(track, crs = polar.stereo)
shp <- st_transform(shp, crs = 3995)
shp <- st_read(filelist[1])
shp <- st_transform(shp, crs = polar.stereo)

plot(st_geometry(shp))
plot(st_geometry(track), add = TRUE)

plot(st_geometry(track[1]), col = 'red', add = TRUE)
plot(st_geometry(track[1,]), col = 'red', add = TRUE)
plot(st_geometry(shp), add = TRUE)

lsfilelist <- as.list(filelist)
file <- filelist[grep("20060821", filelist)]

filelist[which=="20060821"]
# create list of shapefiles 

sptest <- readOGR(filelist[[1]]) # test: sp seems to preserve file info better than sf
sftest <- st_read(filelist[[1]])
rastershp <- shapefile(filelist[[1]])

shp.list <- list()
for (i in 1:length(filelist)){
  shp.list[[i]] <- shapefile(filelist[i])
}

# separate date component of POLY name to correspond to spdf metadata 

sptest

# find out where name/date is in shp metadata
shptest$geometry$

shp.list <- list()
date<-vector()
for (i in 1:length(filelist)) {
  shp.list[[i]]<-st_read(filelist[i])
  tt<-unlist(strsplit(names(stack[[i]]), "[.]"))
  date[i] <-tt[which(nchar(tt)==max(nchar(tt)))]
}