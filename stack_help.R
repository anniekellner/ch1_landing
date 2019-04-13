#################################################
### FOR STACK OVERFLOW ##########################
#################################################

rm(list = ls())

library(sp)
library(sf)
library(raster)
library(dplyr)
library(SDMTools)

load('all_v2.RData')
load('Ice_Measurements.RData')

# Select two different bears

start <- subset(all.v2, start.swim==1)
start <- start %>% # remove 'undecided'
  filter(!(id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004'))
start <- droplevels(start)

# Create simple dataframe

mydata <- select(ice.df, animal, X, Y, ordinal)
mydata <- filter(mydata, animal=='pb_06817' | animal=='pb_20414')
mydata2 <- slice(mydata, 1:2, 760:761 )

mydata <- read.table(header=TRUE, text = "
                         animal        X       Y ord.year
1 pb_20414 157978.9 2323819     2009168
2 pb_20414 156476.3 2325586     2009168
3 pb_06817 188512.0 2299679     2006263
4 pb_06817 207270.9 2287248     2006264")

# Create SpatialPointsDataFrame
projection <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(ice.df$X, ice.df$Y)
mydata.spdf <- SpatialPointsDataFrame(coords = coords, data = mydata, proj4string = projection)

rasterlist <- list.files('./masie_ice_r00_v01_2009168_4km.tif', './masie_ice_r00_v01_2006263_4km.tif', './masie_ice_r00_v01_2006264_4km.tif', full.names = TRUE)
