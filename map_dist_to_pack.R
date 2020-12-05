###################################################
###     Map of Distance to Pack Ice   #############
###################################################

library(dplyr)
library(sf)
library(stars)
library(tmap)

load('coxph.RData') #GPS data

setwd("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/start_swim")

swim <- subset(bears, start.swim == 1)
swim <- distinct(swim)

# Create spatial object

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(swim$X, swim$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=swim, proj4string = projection)
swim.sf <- st_as_sf(pb.spdf)

plot(st_geometry(swim.sf))

# Load spatial data

load("rasters.RData")

min <- st_as_stars(pack[[1]])
min <- na_if(min, "FALSE") # eliminate ice not connected to pack

min_shp <- st_read("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/POLY/asi-AMSR2-n3125-20120815-v5.4.shp") 

max <- st_as_stars(pack[[14]])

min <- st_transform(min, st_crs(swim.sf))
min_shp <- st_transform(min_shp, st_crs(swim.sf))

# Bounding box
bb.swim <- tmaptools::bb(swim.sf, width = 2, height = 3, relative = TRUE)
bb.swim2 <- tmaptools::bb(bb.swim, ylim = c(0.25,2), relative = TRUE)


# Make map

library(rnaturalearth)
library(rnaturalearthdata)

nor_america <- ne_countries(continent = 'north america', returnclass = 'sf')
nor_america <- st_transform(nor_america, st_crs(swim.sf))

asia <- ne_countries(continent = 'asia', returnclass = 'sf')
asia <- st_transform(asia, st_crs(swim.sf))


tm_shape(min, bbox = bb.swim2) + 
  tm_raster(legend.show = FALSE) + 
  tm_shape(swim.sf, bbox = bb.swim2) + 
  tm_dots(size = 0.25) + 
  tm_shape(nor_america, bbox = bb.swim2) + 
  tm_fill(col = "#9CD3AA")

vignette('rnaturalearth', package='rnaturalearth')
  
