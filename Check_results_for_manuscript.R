#################################################################################################
##################  CHECK PREVIOUS WORK FOR CONSISTENCY   #######################################
#################################################################################################

# This document is for checking work done in previous years (e.g., before redefining departure dates) against current work.

rm(list = ls())

library(sf)
library(dplyr)

# Check start.swim points against results in Migration_notebook.Rmd.


load("logreg.RData")
land <- st_read('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears_GIS/Land Shapefiles/AK_CA_5kbuff.shp')
land.dissolve <- land %>% summarise(Shape_Area = sum(Shape_Area))
land.crop <- st_crop(land.dissolve, xmin = -596311, ymin = 2083889, xmax = 850962, ymax = 3576142) # crop shapefile


ss <- subset(logreg, start.swim == 1)
ss <- st_transform(ss, crs = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")


plot(st_geometry(land.crop))
plot(st_geometry(ss), add = TRUE)

st_crs(land.crop)
st_crs(ss)

