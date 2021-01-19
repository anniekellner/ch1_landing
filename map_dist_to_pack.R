###################################################
###     Map of Distance to Pack Ice   #############
###################################################

rm(list = ls())

library(dplyr)
library(sf)
library(tmap)
library(raster)
library(rgdal)

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
#writeOGR(poly, dsn = './shapefiles', layer = 'asi-AMSR2-n3125-20120815-v5.4.tif.shp', driver = 'ESRI Shapefile')

min <- st_read('./shapefiles/asi-AMSR2-n3125-20120815-v5.4.tif.shp.shp')
min <- sf::st_transform(min, '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')
min <- sf::st_transform(min, sf::st_crs(swim.sf))

# Median ice extent

med <- sf::st_read('./shapefiles/asi-n3125-20050819-v5.4.tif.shp')
med <- sf::st_transform(med, sf::st_crs(swim.sf))

# 1st Quartile

q25 <- sf::st_read('./shapefiles/asi-n3125-20060914-v5.4.tif.shp')
q25 <- sf::st_transform(q25, sf::st_crs(swim.sf))

# 3rd quartile

q75 <- sf::st_read('./shapefiles/asi-AMSR2-n3125-20140811-v5.4.shp')
q75 <- sf::st_transform(q75, sf::st_crs(swim.sf))

# ------------ Make map ------------------------------------------------------------------ #

library(rnaturalearth)
library(rnaturalearthdata)

nor_america <- ne_countries(continent = 'north america', returnclass = 'sf')
nor_america <- sf::st_transform(nor_america, sf::st_crs(swim.sf))

# Bounding box
bb.swim <- tmaptools::bb(swim.sf, width = 2, height = 3, relative = TRUE)
bb.swim2 <- tmaptools::bb(bb.swim, ylim = c(0.3, 1.75), relative = TRUE)

# Map

map <- tm_shape(max, bbox = bb.swim2) +
  tm_fill("#95BAC8") + 
  tm_shape(med) + 
  tm_fill(col = "#8c96c6") + 
  tm_shape(min) + 
  tm_fill(col = "#810f7c") +
  tm_shape(nor_america) + 
  tm_fill('#9CD3AA') +
  tm_shape(swim.sf) + 
  tm_dots(size = 0.3) + 
  tm_compass(position = "left") + 
  tm_layout(main.title = "Migration Departure Points Relative to Pack Ice", main.title.position = "center",
            legend.outside = TRUE) + 
tm_add_legend(type = "fill", labels = c("minimum (2012)", "median (2005)", "maximum (2009)"), col = c("#810f7c","#8c96c6","#95BAC8"), border.col = "black", title = "Pack Ice Extent")

tmap_save(map, 'C:/Users/akell/Documents/PhD/Polar_Bears/Figures/pack.png')
