rm(list = ls())

library(adehabitatHR)
library(dplyr)

load('land_bears_CoxPH.RData')

#set up data

use <- subset(bears, month == 6 | month==7 | month==8 | month==9)
use <- dplyr::select(use, gps_lat, gps_lon)

#convert to grid
library(proj4)
M <- as.matrix(cbind(use$gps_lon, use$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]

use2 <- cbind(use,X,Y)
XY <- select(use2, X,Y)

library(sp)
library(rgdal)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(XY$X, XY$Y)
XY.sp <- SpatialPoints(XY, proj4string = projection)

ca <- mcp(XY.sp, percent=95) #95% core area - can change to any percent
plot(ca)

#write to shapefile
writeOGR(ca, dsn = './Shapefiles', layer ="95ca_ows_land_bears", driver='ESRI Shapefile')

