rm(list = ls())
library(dplyr)
library(sp)
library(rgdal)

load('all.RData')
end <- subset(all, end.swim==1)

#create spdf
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(end$X, end$Y)
end.spdf <- SpatialPointsDataFrame(coords=coords, data=end, proj4string = projection)

#save shp to GIS folder
writeOGR(end.spdf, dsn = 'C:/Users/akell/Documents/ArcGIS/Polar_Bears_GIS/Location Data', layer ="end_swim", driver='ESRI Shapefile')
