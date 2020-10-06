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


load('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/SIC_new.RData')

new <- subset(all_SIC_new, all_SIC_new$id == "pb_20333.2008" | all_SIC_new$id == "pb_20525.2014")

setwd('C:/Users/akell/Documents/ArcGIS')

# ---- FUNCTIONS  -------------------------------------------------------------------------------------------------------- #

st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}


# LOAD DATA --------------------------------------------------------------------------------- #

# create spdf using sp

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +x_0=0 +y_0=0 +a=6378273 +rf=298.279411123064 +units=m +no_defs') # matches AMSRE raster

coords <- cbind(new$X, new$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = new, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

pb <- st_as_sf(pb.spdf.polar)

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
land.ps <- st_transform(land.crop, polar.stereo)

# Verify land and points look good

tm_shape(pb) + # plot points first so frame is large enough to accompany points and land
  tm_dots(col = "month", popup.vars = "ymd") +
  tm_shape(land.ps) +
  tm_fill()
  

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

dist <- vector()
dist <- st_distance(pb, land.ps, by_element = TRUE)

pb$dist2land <- cbind(matrix(dist))


# --  ADD TO EXISTING DISTANCE DATA ----------------------------------------------- #

load('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/logreg.RData')
load('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/all_v2.RData')

lb <- subset(all.v2, land_bear == 1)

ss <- subset(lb, start.swim == 1) # 18 swims after adding data below
ss <- unique(ss$id)

bears <- subset(lb, lb$id %in% ss)
bears <- filter(bears, month > 5 & month < 10)

SIC <- all_SIC_new %>%
  dplyr::select(id.datetime, SIC_30m_me:SIC_30m_min) 

bears <- bears %>%
  left_join(SIC, by = "id.datetime")


dist1 <- logreg %>%
  dplyr::select(id.datetime, dist2land) %>%
  st_drop_geometry()


dist2 <- pb %>%
  dplyr::select(id.datetime, dist2land) %>%
  st_drop_geometry() %>%
  bind_rows(dist1)


bears <- bears %>%
  left_join(dist2, by = "id.datetime")


save(bears, file = 'C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/land_bears_CoxPH.RData')



