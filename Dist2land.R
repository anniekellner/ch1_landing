###############################################
#####   Distance to land    ###################
###############################################

# Calculate distance from land bears to land
# Need to merge land shapefile into one multipolygon, or distances are incorrect

rm(list = ls())

library(sf)
library(sp)
library(dplyr)
library(tmap)
library(lwgeom)
library(proj4)
library(raster)

setwd('C:/Users/akell/Documents/ArcGIS')

# LOAD DATA --------------------------------------------------------------------------------- #

load('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/land_bears_ows.RData')

#dem <- raster('C:/Users/akell/Documents/ArcGIS/North_Slope_DEM/DEM_052520/DEM_052520/ans_dem_8bit.tif')
#plot(dem)
#dem.poly <- rasterToPolygons(dem, na.rm = TRUE) # took way too long; crashed memory

# check
#tm_shape(dem) +
  #tm_raster(legend.show = FALSE) +
  #tm_shape(bbox) + 
  #tm_polygons()

bbox <- st_read('./Land Shapefiles/bbox.shp')

polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') 

land <- st_read('./Land Shapefiles/AK_CA_5kbuff.shp')
land.dissolve <- land %>% summarise(Shape_Area = sum(Shape_Area)) # dissolve AK and territories into one obj
plot(st_geometry(land.dissolve)) # OK

# see where bbox overlaps shapefile

tm_shape(land.dissolve) +
  tm_polygons() +
  tm_shape(bbox) +
  tm_polygons()

land.crop <- st_crop(land.dissolve, xmin = -596311, ymin = 2083889, xmax = 850962, ymax = 3576142) # crop shapefile
land.ps <- st_transform(land.crop, polar.stereo)

# Verify land and points look good

tm_shape(pb) + # plot points first so frame is large enough to accompany points and land
  tm_dots() +
  tm_shape(land.ps) +
  tm_fill()
  

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

dist <- vector()
dist <- st_distance(pb, land.ps, by_element = TRUE)

pb$dist2land <- cbind(matrix(dist))

save(pb, file = 'C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/land_bears_ows.RData')

# -------------------------------------------------------------------------- #

# check first ten points

test <- pb[1:10,]


library(tmap)
tmap_mode("view")

land.map <- tm_shape(land.ps) +
  tm_polygons() 

land.map +
tm_shape(test) + 
  tm_dots(col = 'day')
  

