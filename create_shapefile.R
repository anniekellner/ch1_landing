# project lat-long to Alaska Albers

library(proj4)

M <- as.matrix(cbind(st$gps_lon, st$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]

#bind the datasets
loc2 <- cbind(st,X,Y)

library(sp)
library(rgdal)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(loc2$X, loc2$Y)
spdf.use <- SpatialPointsDataFrame(coords = coords, data = loc2, proj4string = projection)

#create shapefile (Spaital Point DataFrame for the use locations for extraction)

writeOGR(spdf.use,dsn = "C:/Users/akell/Documents/ArcGIS/Polar_Bears_GIS/Location Data", layer = "start_swim", driver = "ESRI Shapefile")

# Create shapefile for new departure-from-ice dates


