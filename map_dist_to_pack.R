###################################################
###     Map of Distance to Pack Ice   #############
###################################################

rm(list = ls())

library(dplyr)
library(sf)
library(stars)
library(tmap)
library(raster)

setwd("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/start_swim")


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


# Stats 

size <- vector()

for(i in 1:length(pack)){
  size[i] <- cellStats(pack[[i]], 'sum')
}

quantile(size)

size_sort <- sort(size)


# minimum raster

min_ras <- raster('./rasters/ice_binary/asi-AMSR2-n3125-20120815-v5.4.tif')
gv <- getValues(min_ras)
mode <- modal(gv, na.rm = TRUE) # 526


m <- c(0,526,0,526,527,1, 527,Inf,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

rc_min <- reclassify(min_ras, rclmat) 
rcc_min <- clump(rc_min, directions=8)

# maximum raster

max_ras <- raster('./rasters/ice_binary/asi-n3125-20090718-v5.4.tif')
gv <- getValues(max_ras)
mode <- modal(gv, na.rm = TRUE) # 125

m <- c(0,125,0, 125,126,1, 126,100000,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)

rc_max <- reclassify(max_ras, rclmat) 

# Median raster

med_ras <- raster('./RCC/asi-n3125-20050819-v5.4.tif')
gv <- getValues(med_ras)
mode <- modal(gv, na.rm = TRUE) # 216

m <- c(0,216,0, 216,217,1, 217,100000,0) 
rclmat <- matrix(m, ncol=3, byrow=TRUE)

rc_med <- reclassify(med_ras, rclmat) 

all_ras <- rc_min + rc_max
plot(all_ras)


test <- pack[[1]] + pack[[14]]


#rclmat <- matrix(m, ncol=2, byrow=TRUE)

#for(i in 1:length(pack)){
  #new <- reclassify()

min <- st_as_stars(pack[[1]])
min <- na_if(min, "FALSE") # eliminate ice not connected to pack
#min <- st_transform(min, st_crs(swim.sf))

max <- st_as_stars(pack[[14]])
max <- na_if(max, "FALSE") # eliminate ice not connected to pack
#max <- st_transform(max, st_crs(swim.sf))



q25 <- st_as_stars(pack[[11]])
q25 <- na_if(q25, "FALSE")
#q25 <- st_transform(q25, st_crs(swim.sf))

q50 <- st_as_stars(pack[[9]])
q50 <- na_if(q50, "FALSE")
q50 <- st_transform(q50, st_crs(swim.sf))

q75 <- st_as_stars(pack[[5]])
q75 <- na_if(q75, "FALSE")
#q75 <- st_transform(q75, st_crs(swim.sf))

# Combine rasters



test <- recode(stack, 1 == "TRUE")

# Make map

library(rnaturalearth)
library(rnaturalearthdata)

nor_america <- ne_countries(continent = 'north america', returnclass = 'sf')
nor_america <- st_transform(nor_america, st_crs(swim.sf))


# Bounding box
bb.swim <- tmaptools::bb(swim.sf, width = 2, height = 3, relative = TRUE)
bb.swim2 <- tmaptools::bb(bb.swim, ylim = c(0.2, 2), relative = TRUE)

tm_shape(swim.sf, bbox = bb.swim2) + # test whether raster works individually
  tm_dots() + 
  tm_shape(min) + 
  tm_raster()

tm_shape(swim.sf, bbox = bb.swim2) + 
  tm_dots() + 
  tm_shape(nor_america) +
  tm_fill(col = "#9CD3AA")


tm_shape(swim.sf, bbox = bb.swim2) + 
  tm_dots() +
  #tm_shape(nor_america) + 
  #tm_fill(col = "#9CD3AA") + 
  tm_shape(max) + 
  tm_raster(col = "#edf8fb", legend.show = FALSE) + 
  tm_shape(min, bbox = bb.swim2) + 
  tm_raster(col = "#810f7c", legend.show = FALSE)




tm_shape(max, bbox = bb.swim2) + 
  tm_raster() + 
  tm_shape(swim.sf, bbox = bb.swim2) +
  tm_dots()


  
