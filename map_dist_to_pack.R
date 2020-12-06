###################################################
###     Map of Distance to Pack Ice   #############
###################################################

rm(list = ls())

library(dplyr)
library(sf)
library(stars)
library(tmap)
library(raster)

load('coxph.RData') #GPS data

#setwd("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/start_swim") # Home Computer
setwd("D:/Polar Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/start_swim")

swim <- subset(bears, start.swim == 1)
swim <- distinct(swim)

# Create spatial object

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(swim$X, swim$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=swim, proj4string = projection)
swim.sf <- st_as_sf(pb.spdf)

plot(st_geometry(swim.sf))

# Load spatial data

load("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/start_swim/rasters.RData")

filelist <- dir(path = "./rasters/RCC", pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=TRUE)

# Max ice extent

max <- sf::st_read('./shapefiles/asi-n3125-20090718-v5.4.tif.shp')
max <- sf::st_transform(max, sf::st_crs(swim.sf))

# Min ice extent

#r <- raster('D:/Polar Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/start_swim/rasters/RCC/asi-AMSR2-n3125-20120815-v5.4.tif')
#gv <- getValues(r) # change values to vector so can get mode
#mode <- modal(gv, na.rm=TRUE) # find mode
#poly <- rasterToPolygons(r, function(x){x==mode}, dissolve = TRUE) #raster to polygon
#writeOGR(poly, dsn = './shapefiles', layer = r, driver = 'ESRI Shapefile')
  

# Median ice extent

med <- sf::st_read('./shapefiles/asi-n3125-20050819-v5.4.tif.shp')
med <- sf::st_transform(med, sf::st_crs(swim.sf))

# 1st Quartile

q25 <- sf::st_read('./shapefiles/asi-n3125-20060914-v5.4.tif.shp')
q25 <- sf::st_transform(q25, sf::st_crs(swim.sf))

# 3rd quartile

q75 <- sf::st_read('./shapefiles/asi-AMSR2-n3125-20140811-v5.4.shp')
q75 <- sf::st_transform(q75, sf::st_crs(swim.sf))

# Make map

library(rnaturalearth)
library(rnaturalearthdata)

nor_america <- ne_countries(continent = 'north america', returnclass = 'sf')
nor_america <- sf::st_transform(nor_america, sf::st_crs(swim.sf))


# Bounding box
bb.swim <- tmaptools::bb(swim.sf, width = 2, height = 3, relative = TRUE)

bb.swim2 <- tmaptools::bb(bb.swim, ylim = c(0.23, 2), relative = TRUE)

tm_shape(swim.sf, bbox = bb.swim2) + # works
  tm_dots() + 
  tm_shape(nor_america) + 
  tm_fill('#9CD3AA') +
  tm_shape(min) + 
  tm_polygons(col = "#810f7c") + 
  tm_shape(max) + 
  tm_polygons(col = "#edf8fb") + 
  tm_shape(med) + 
  tm_polygons(col = "#8c96c6") + 
  tm_shape(q25) + 
  tm_polygons(col = "#b3cde3") + 
  tm_shape(q75) + 
  tm_polygons(col = "#8856a7")
  


  
