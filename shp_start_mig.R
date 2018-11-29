rm(list = ls())

## add XY coordinate data to 'all' database (proj=Alaska Albers)

library(proj4)

load('all.RData')

M <- as.matrix(cbind(all$gps_lon, all$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]
all <- cbind(all,X,Y)
save(all, file='all.RData')

# Create .shp for departure-from-ice locations

library(sp)
library(rgdal)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org

start <- subset(all, start.swim==1) #subset data

coords <- cbind(start$X, start$Y)
spdf.use <- SpatialPointsDataFrame(coords = coords, data = start, proj4string = projection) 

#create shapefile (Spaital Point DataFrame for the use locations for extraction)

writeOGR(spdf.use,dsn = "C:/Users/akell/Documents/ArcGIS/Polar_Bears_GIS/Location Data", layer = "start_migration", driver = "ESRI Shapefile")