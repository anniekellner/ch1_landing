rm(list = ls())
library(adehabitatHR)
library(dplyr)

load('all.RData')

#set up data

use <- subset(all, month==7 | month==8 | month==9)
use <- dplyr::select(use, gps_lat, gps_lon)

#convert to grid
library(proj4)
M <- as.matrix(cbind(use$gps_lon, use$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]

use2 <- cbind(use,X,Y)
XY <- select(use2, X,Y)
xysp <- SpatialPoints(XY) # eliminate ID, getting 95% core area for all bears

cp <- mcp(xysp, percent=95) #95% core area
plot(cp)

#write to shapefile
library(maptools)
writeOGR(cp, dsn = './Shapefiles', layer ="95ca_ows_all", driver='ESRI Shapefile')

writeOGR(spdf.use,dsn = "./Output", layer = "all_locations", driver = "ESRI Shapefile")