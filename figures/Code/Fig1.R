##########################################
##    Chapter 1 Fig 1   ##################
##########################################


rm(list = ls())

library(ggOceanMaps)
library(sf)
library(ggplot2)
library(ggsn)
library(dplyr)
library(sp)
library(svglite)

# Projections

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar <- CRS("+proj=stere +lat_0=90 +lat_ts=71 +lon_0=-152.5 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")


# DEM

dem_poly <- st_read('C:/Users/akell/Documents/ArcGIS/North_Slope_DEM/dem_poly/dem_poly.shp')

land <- dem_poly %>%
  filter(gridcode != 27) %>%
  st_union() 


# Buffer

buf <- st_read('C:/Users/akell/Documents/ArcGIS/Land Shapefiles/AK_CA_5kbuff.shp')

buf_proj <- st_transform(buf, st_crs(dem_poly))

buf_crop <- st_crop(buf_proj, dem_poly)

# Landing Points

all.v2 <- readRDS('./data/derived-data/all_v2.Rds') # to get landing points

end.swim <- all.v2 %>%
  dplyr::filter(end.swim == 1) %>%
  dplyr::select(animal, year, month, day, hour, minute, second, gps_lat, gps_lon, datetime, start.swim, end.swim, id, X, Y, ymd, id.datetime)

coords.end <- cbind(end.swim$X, end.swim$Y) 
end.sp <- SpatialPointsDataFrame(coords = coords.end, data = end.swim, proj4string = projection)
end.sf <- st_as_sf(end.sp)

end.sf <- st_transform(end.sf, crs = polar)

centroid_landing <- st_coordinates(end.sf) %>%
  as.data.frame() %>%
  summarise(across(.cols = everything(), .fns = mean)) %>%
  SpatialPoints() %>%
  st_as_sf() %>%
  st_set_crs(polar)


# Starting points

load('coxph.RData') #GPS data
swim <- subset(bears, start.swim == 1)
swim <- distinct(swim)


coords <- cbind(swim$X, swim$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=swim, proj4string = projection)
swim.sf <- st_as_sf(pb.spdf)

swim.sf <- st_transform(swim.sf, crs = polar)

centroid_starting <- st_coordinates(swim.sf) %>%
  as.data.frame() %>%
  summarise(across(.cols = everything(), .fns = mean)) %>%
  SpatialPoints() %>%
  st_as_sf() %>%
  st_set_crs(polar)

# Create map

main <- basemap(limits = c(-165, -140, 70, 73), rotate = TRUE, bathymetry = TRUE, bathy.style = "poly_blues", land.col = "#9ECBA0") + 
  theme(legend.justification = "top") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key = element_blank()) +
  geom_sf(data = buf_crop, fill = "purple", alpha = 0.5) + 
  geom_sf(data = land, fill = "#9ecba0") + 
  annotation_scale(location = 'bl') +
  geom_sf(data = end.sf, color = "#EA801B") + 
  geom_sf(data = swim.sf, color = "red") + 
  geom_sf(data = centroid_landing, shape = 23, size = 3, color = "#EA801B") +
  geom_sf(data = centroid_starting, shape = 23, size = 3, color = "red")

ggsave("Fig1_R.svg", plot = main, path = 'C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Dissertation/Chapter 1/Figures')

# -------------------------------------------------------------------------------------------------------------------------- #
# Which arrival is missing

ids.start <- unique(swim$id)
ids.end <- unique(end.sf$id)

setdiff(ids.start, ids.end) # pb_20525.2014, pb_20333.2008

missing_arr1 <- subset(all.v2, id == "pb_20333.2008" & month == 8 & day == 30 & hour == 12)

all.v2[11585,]["end.swim"] <- 1

saveRDS(all.v2, file = './data/derived-data/all_v2.Rds')


