################################################################
##    LAND-ICE COMPARISON: MATCH BEARS      ####################
################################################################

# Projection: Albers Equal Area Conic

rm(list = ls())

library(dplyr)
library(sf)
library(ggplot2)


# load data

load('all_v2.RData')
load('ded_ids.RData')

# spatial data - start points for land bears and buffer

st <- subset(all.v2, all.v2$id %in% ded)
st <- subset(st, start.swim ==1)

albers.proj <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") # Albers Equal Area Conic (same as ArcMap doc)
start.sf.wgs <- st_as_sf(st, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY') # need to import sf object as lat/long first if that's what data is recorded as
start.sf.alb <- st_transform(start.sf.wgs, albers.proj) # then transform

plot(st_geometry(start.sf.alb))  # verify points look good
buf <- st_buffer(start.sf.alb, dist = 30000) # 30 km buffer
plot(buf) # make sure buffers look good

# spatial data - all ows

ows <- subset(all.v2, month == 6 | month == 7 | month == 8 | month == 9 | month == 10)
ows.sf.wgs <- st_as_sf(ows, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
ows.alb <- st_transform(ows.sf.wgs, albers.proj)
plot(st_geometry(ows.alb)) # check points

# plot to make sure projections match and points/buffers look good

ggplot() +
  geom_sf(data = ows.alb) +
  geom_sf(data = buf, fill = "grey") +
  geom_sf(data = start.sf.alb, color = "red")

# Find points within buffers

pb1.buf <- st_intersection(buf[1,], ows.alb) # pb_06817
plot(st_geometry(pb1.buf))
pb1.buf <- select(pb1.buf, animal.1:geometry) 
pb1.buf <- subset(pb1.buf, year.1 == 2006 & animal.1 !='pb_06817')
plot(st_geometry(pb1.buf))

unique(pb1.buf$animal.1) # 20413, 20479. 20413 is a land bear

# Plot similar bears



bears2006 <- subset(ows.alb, id == 'pb_06817.2006' | id == 'pb_20413.2006' | id == 'pb_20479.2006')
bears2006.df <- as.data.frame(bears2006)
bears2006.df <- droplevels(bears2006.df)


bears2006.sf <- st_as_sf(bears2006.df)

# Plot bears

library(tmap)
tmap_mode("plot")
tm_shape(bears2006.sf) + tm_symbols(col = "animal", shape = "month", palette = sf.colors(3))

st_write(bears2006.sf, dsn = 'C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears_GIS/Location Data', layer = 'compare2006', driver = 'ESRI Shapefile', update = TRUE)



