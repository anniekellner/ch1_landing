###############################################
#####   Distance to land  - land bears  ###################
###############################################

# Calculate distance from land bears to land
# Need to merge land shapefile into one multipolygon, or distances are incorrect
# Calculated in Alaska Albers projection

rm(list = ls())

library(sf)
library(sp)
library(dplyr)
library(tmap)
library(lwgeom)
library(proj4)
library(raster)
library(tidyr)

mydata <- readRDS('./data/RData/land_bears_cutoff_after_swim.Rds')

load("land_bears_CoxPH.RData")

setwd('C:/Users/akell/Documents/ArcGIS')


# LOAD DATA --------------------------------------------------------------------------------- #

# create spdf using sp

#projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
#polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +x_0=0 +y_0=0 +a=6378273 +rf=298.279411123064 +units=m +no_defs') # matches AMSRE raster

#coords <- cbind(mydata$X, mydata$Y)
#pb.spdf <- SpatialPointsDataFrame(coords = coords, data = mydata, proj4string = projection) 
#pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

#pb <- st_as_sf(pb.spdf.polar)

bbox <- st_read('./Land Shapefiles/bbox.shp')


land <- st_read('./Land Shapefiles/AK_CA_5kbuff.shp')
land.dissolve <- land %>% summarise(Shape_Area = sum(Shape_Area)) # dissolve AK and territories into one obj
plot(st_geometry(land.dissolve)) # OK

# see where bbox overlaps shapefile

tm_shape(land.dissolve) +
  tm_polygons() +
  tm_shape(bbox) +
  tm_polygons()

land.crop <- st_crop(land.dissolve, xmin = -596311, ymin = 2083889, xmax = 850962, ymax = 3576142) # crop shapefile
land.ps <- st_transform(land.crop, st_crs(mydata))

# Verify land and points look good

tmap_mode("view")

tm_shape(mydata) + # plot points first so frame is large enough to accompany points and land
  tm_dots(col = "month", popup.vars = "ymd") +
  tm_shape(land.ps) +
  tm_fill()
  

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

dist <- vector()
mydata$dist_to_land <- st_distance(mydata, land.ps, by_element = TRUE)

pb$dist2land <- cbind(matrix(dist))
pb$dist2land <- as.numeric(pb$dist2land)


# --  ADD TO EXISTING DISTANCE DATA ----------------------------------------------- #

pb.sel <- pb %>%
  st_drop_geometry()

bears <- rbind(bears, pb.sel)
bears <- drop_na(test, dist2land)

save(bears, file = 'C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/land_bears_CoxPH.RData')



