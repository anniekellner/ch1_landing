load('all.RData')

swim <- subset(all, start.swim==1|end.swim==1)


# project lat-long to Alaska Albers

library(proj4)

M <- as.matrix(cbind(swim$gps_lon, swim$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]

#bind the datasets
swim <- cbind(swim,X,Y)

library(sp)
library(rgdal)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(swim$X, swim$Y)
spdf.use <- SpatialPointsDataFrame(coords = coords, data = swim, proj4string = projection)

#create shapefile (Spaital Point DataFrame for the use locations for extraction)

writeOGR(spdf.use,dsn = "./Shapefiles", layer = "swim", driver = "ESRI Shapefile")