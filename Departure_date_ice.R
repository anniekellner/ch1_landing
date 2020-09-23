##################################################################
#####     Defining Departure Dates for Ice Bears  #################
##################################################################

rm(list = ls())

library(dplyr)
library(tidyr)
library(tmap)
library(ggplot2)
library(lubridate)
library(sf)
library(plotly)
library(raster)
library(stars)
library(rgdal)

load('all_v2.RData') # Has correct repro info
load('logreg.RData') # Not sure whether has correct repro info. Is sf object 

# ----  Data Prep  ------------------ #

# Remove bears for which there is little data

ice <- all.v2 %>%
  inner_join(logreg) %>%
  filter(month == 5 |month == 6 | month == 7 | month == 8) %>%
  filter(land_bear == 0) 

unique(ice$id) # 107 ice bears - includes ice bears that were prevoiusly excluded during formation of logreg df

insufPoints <- ice %>% # remove bears with < 100 data points
  group_by(id) %>%
  add_count(id) %>%
  filter(n < 100)

ice <- anti_join(ice, insufPoints)

unique(ice$id) # 96 bears

# Visualize using tmap

#tmap_mode("view")

#tm_shape(ice.sf) + 
  #tm_symbols(col = "month", popup.vars = "datetime")  + 
  #tm_facets(by = "id") +
  #tmap_options(limits = c(facets.view = 100))

# Remove bears with insufficient data via visual inspection from tmap

insufData <- c('pb_20794.2005', "pb_20925.2009", "pb_21221.2010", "pb_21283.2012", "pb_21291.2012", "pb_21296.2012", "pb_21309.2012", "pb_32799.2006",
  "pb_20296.2015", "pb_20449.2014", "pb_20713.2004", "pb_20741.2008", "pb_20753.2015", "pb_20794.2006", "pb_20965.2008", "pb_20974.2008", "pb_20982.2008",
  "pb_21015.2012", "pb_21015.2014")

insuf <- subset(ice, ice$id %in% insufData)

ice <- ice %>% anti_join(insuf)

lat <- ice %>% 
  select(id, ymd, gps_lat) %>%
  ungroup()
  

unique(lat$id) # 83 bears

# ----  PLOTTING  ----------------------------------------------------------------------------------------------------------------------- #

# Explore: graph change in latitude 
# x axis = time; y axis = latitude

lat <- lat %>% 
  mutate(ordinal = yday(ymd)) %>%
  group_by(id, ordinal) %>%
  summarise(avgLat = mean(gps_lat)) %>%
  ungroup()

pb_06336 <- lat %>%
  filter(id == "pb_06336.2004")

p <- ggplot(lat, aes(x = ordinal, y = avgLat, col = id)) + 
  geom_line() + 
  facet_wrap(~ id)

ggplot(pb_06336, aes(x = ordinal, y = avgLat)) + 
  geom_line()


fig <- ggplotly(p) # makes interactive plot
fig

# ------- CASE STUDY: pb_06336.2004 ------------------------------------------------------------------------------------------------------------- #

bear <- subset(logreg, id == "pb_06336.2004")

bbox <- st_read("C:/Users/akell/Documents/ArcGIS/Land Shapefiles/bbox.shp") # to crop raster/stars object

bbox <- st_transform(bbox, crs = crs(June1))
bbox <- st_bbox(bbox) # Make into bbox object

June1 <- raster("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/OWS_2004/asi-n3125-20040601-v5.4.tif", package = "stars")
July29 <- raster("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/OWS_2004/asi-n3125-20040729-v5.4.tif", package = "stars")

June1 <- June1 %>%
  st_as_stars() %>%
  st_crop(bbox)
  
plot(June1)

July29 <- July29 %>%
  st_as_stars() %>%
  st_crop(bbox)


tmap_mode("view")

bear$month <- as.character(bear$month)

# June 1 ice

tm_shape(June1, raster.downsample = FALSE) +
  tmap_options(max.categories = 256) +
  tm_raster(alpha = 0.5) +
tm_shape(bear) +
  tm_symbols(size = 0.1, col = "month", popup.vars = c("month", "day"))

# July 29 ice

tm_shape(July29, raster.downsample = FALSE) +
  tmap_options(max.categories = 256) +
  tm_raster(alpha = 0.5) +
  tm_shape(bear) +
  tm_symbols(size = 0.1, col = "month", popup.vars = c("month", "day"))

# Bear 06336: swim distance

bear[304,]

st_distance(bear[304,], bear[305,]) # how long is this ice bear's swim?

# --------------------------------------------------------------------------------------------------------------------------------------- #

# Follow Pagano et al. 2012 to identify swimming events

# STEP ONE: IDENTIFY TIME GAPS
# If no data for >= 3 days. likely collar submersion

diff <- ice %>%
  group_by(id) %>%
  mutate(time_diff = difftime(datetime, lag(datetime), tz = "US/Alaska", units = "days")) %>%
  filter(time_diff >= 3) %>%
  select(id, datetime, gps_lat, gps_lon, distance, rate, time_diff) %>%
  ungroup()

write.csv(diff, file = "C:/Users/akell/Desktop/ice_bear_time_diff.csv")

# To ID swims: compare GPS locations with available satellite imagery of sea ice presence, concentration, and extent

# To look for directional movements north:  
  # Categorize Northward v. non-Northward movements 
  # Visualize

az <- ice %>%   # Azimuth
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(North = if_else(azimuth < 56.25 | azimuth > 281.25, 1, 0)) %>%
  select(id, ymd, distance, North, geometry) %>%
  ungroup()

az <- st_as_sf(az)

tmap_mode("view")

tm_shape(az) +
  tm_symbols(col = "North", popup.vars = "ymd") +
  tm_facets(by = "id") + 
  tmap_options(limits= c(facets.view = 100))

# Flag columns with > 10 North points in a row


North = az %>%
  mutate(cumdistNorth = North * distance) %>%
  group_by(id, grp = cumsum(North == 0)) %>%
  mutate(cumdistNorth = cumsum(cumdistNorth)) %>%
  ungroup() %>%
  select(-grp)


North = az %>%
  group_by(id, grp = cumsum(North == 0)) %>%
  mutate(cumNorth = cumsum(North)) %>%
  ungroup() %>%
  select(-grp)
           


  
  
   


# Plan: 

# Identify bears that swim north
# Look at cub survival - use only bears with repro data?
 



# Should fill in missing points - interpolate using CRAWL or adehabitat?

# Classify movement as northward (WNW (315 - ))

# classification: ESE (101.25) - WSW (258.75)  

swim.az$south <- ifelse(swim.az$azimuth > 101.25 & swim.az$azimuth < 258.75, 1, 0)

