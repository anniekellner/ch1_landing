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

min <- st_as_stars(pack[[1]])
min <- na_if(min, "FALSE") # eliminate ice not connected to pack
min <- st_transform(min, st_crs(swim.sf))

max <- st_as_stars(pack[[14]])
max <- na_if(max, "FALSE") # eliminate ice not connected to pack
max <- st_transform(max, st_crs(swim.sf))


q25 <- st_as_stars(pack[[11]])
q25 <- na_if(q25, "FALSE")
q25 <- st_transform(q25, st_crs(swim.sf))

q50 <- st_as_stars(pack[[9]])
q50 <- na_if(q50, "FALSE")
q50 <- st_transform(q50, st_crs(swim.sf))

q75 <- st_as_stars(pack[[14]])
q75 <- na_if(q75, "FALSE")
q75 <- st_transform(q75, st_crs(swim.sf))


# Bounding box
bb.swim <- tmaptools::bb(swim.sf, width = 2, height = 3, relative = TRUE)
bb.swim2 <- tmaptools::bb(bb.swim, ylim = c(0.2, 2), relative = TRUE)

tm_shape(swim.sf, bbox = bb.swim) + 
  tm_dots()

tm_shape(swim.sf, bbox = bb.swim2) + 
  tm_dots() +
  tm_shape(nor_america) + 
  tm_fill(col = "#9CD3AA" )


# Make map

library(rnaturalearth)
library(rnaturalearthdata)

nor_america <- ne_countries(continent = 'north america', returnclass = 'sf')
nor_america <- st_transform(nor_america, st_crs(swim.sf))

tm_shape(max, bbox = bb.swim2) + 
  tm_raster() + 
  tm_shape(swim.sf, bbox = bb.swim2) +
  tm_dots()

tm_shape(min, bbox = bb.swim2) + # this works with min raster
  tm_raster(col = "#7991FA", alpha = 0.5, legend.show = FALSE) + 
  tm_shape(max, bbox = bb.swim2) + 
  tm_raster(col = "#F52636", alpha = 0.2, legend.show = FALSE) + 
  tm_shape(swim.sf) + 
  tm_dots(size = 0.25) + 
  tm_shape(nor_america, bbox = bb.swim2) + 
  tm_fill(col = "#9CD3AA")

tm_shape(max, bbox = bb.swim2) + # this one works with max raster
  tm_raster(col = "#edf8fb", legend.show = FALSE) + 
  tm_shape(swim.sf) +
  tm_dots() + 
  tm_shape(nor_america) + 
  tm_fill(col = "#9CD3AA")
  
