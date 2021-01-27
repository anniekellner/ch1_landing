##################################
###     DISTANCE TO PACK 2021   ##
##################################

rm(list = ls())

library(raster)
library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)

# Load Data

bears <- readRDS('./data/RData/land_bears_CoxPH.Rds') #GPS data

bears <- distinct(bears)
bears <- bears[1:23349,] # last row is NA, not sure why

#sub <- subset(bears, start.swim == 1)

# remove rows after start.swim == 1

bears <- bears %>%
  group_by(id) %>%
  mutate(day = row_number()) 

bears <- bears %>% 
  group_by(id) %>%
  mutate(across(everything(),
                ~replace(., row_number() > match(1, start.swim), NA))) %>%
  ungroup()

bears <- bears %>% select(-id) # remove id row because was interfering with removal of all-NA rows

bears <- bears %>% filter_all(any_vars(!is.na(.))) # filter rows that have > 1 columns with non-missing values

bears$id = paste(bears$animal, bears$year, sep = '.')

filelist <- dir(path = "D:/Polar Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/POLY", pattern='.shp', all.files=TRUE, recursive = TRUE, full.names=TRUE)

# <- str_remove(filelist, ".tif")
filelist <- unique(filelist)

# Create spatial object

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(bears$X, bears$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=bears, proj4string = projection) 
sf <- st_as_sf(pb.spdf)

# Create sf object and project to MASIE CRS

shp <- st_read(filelist[1])
st_crs(shp)

sf <- st_transform(sf, st_crs(shp))

# Visualize data to make sure projections line up

plot(st_geometry(shp))
plot(st_geometry(sf), add = TRUE)

# Create column to match with MASIE raster
sf$date2 <- gsub("-", "", bears$ymd) # format for matching up with dates on GeoTIFFs

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

# Associate GPS point with polygon file name

for (i in 1:nrow(sf)){
  file <- filelist[grep(sf$date2[i], filelist)]
  shp <- st_read(file)
  sf <- st_transform(sf, st_crs(shp))
  sf$dist_to_ice[i] <- st_distance(sf[i,], shp)
}

saveRDS(sf, './data/RData/land_bears_cutoff_after_swim.Rds')

# Check Dist1 and 2 data

load("data/RData/dist1.RData")

sf$dist2pack <- dist
dist1 <- dist
