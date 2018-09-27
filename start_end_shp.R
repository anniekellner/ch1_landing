rm(list = ls())
library(dplyr)

load('all.RData')

start <- subset(all, start.swim==1)
end <- subset(all, end.swim==1)

###############################################
### START #####################################

#convert to grid
library(proj4)
M <- as.matrix(cbind(start$gps_lon, start$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]

start2 <- cbind(start,X,Y)
start2 <- dplyr::select(start2, animal,year,X,Y)  

#create spdf
library(sp)
library(rgdal)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(start2$X, start2$Y)
start.spdf <- SpatialPointsDataFrame(coords=coords, data=start2, proj4string = projection)

#create .shp
writeOGR(start.spdf, dsn = './Shapefiles', layer ="start", driver='ESRI Shapefile')

#######################################
#### END ################################


#convert to grid
library(proj4)
M <- as.matrix(cbind(end$gps_lon, end$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]

end2 <- cbind(end,X,Y)
end2 <- dplyr::select(end2, animal,year,X,Y)  

#create spdf
library(sp)
library(rgdal)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(end2$X, end2$Y)
end.spdf <- SpatialPointsDataFrame(coords=coords, data=end2, proj4string = projection)

#create .shp
writeOGR(end.spdf, dsn = './Shapefiles', layer ="end", driver='ESRI Shapefile')
