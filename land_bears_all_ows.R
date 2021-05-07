###########################################################
###       COMPLETE DATASET    #############################
###########################################################

library(dplyr)
library(tmap)

load('all_v2.RData')
load('ows_land.RData')

ows <- subset(all.v2, ows == 1) # bears with data during ows

land.bears <- ows %>% # create column for number of days spent on land by individual bear
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(days.on.land = cumsum(land))

land.bears <- subset(land.bears, land == 1 & days.on.land > 6) # >= 7 days on land

land.bear.ids <- unique(land.bears$id) # list of bears that spend >=7 days on land


land.bears.ows <- subset(ows, id %in% land.bear.ids) # land bears with data during ows
land.bears.ows <- droplevels(land.bears.ows)

first.entry <- land.bears.ows %>% # Find first entry to see whether on land or ice
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice(1)

start.on.land <- subset(first.entry, land == 1)
start.on.land <- subset(land.bears.ows, id %in% start.on.land$id)

land.bears.all.ows <- anti_join(land.bears.ows, start.on.land) # remove bears collared on land (2008)

# ----    PLOT TO VIEW DATA  ------------------------------------------------------------------ #

# Create spatial object

library(sp)
library(sf)

polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
lb <- DFtoSF(land.bears.all.ows, polar.stereo)

#
tmap_mode("view")

tm_shape(lb) +
  tm_symbols(col = "month", popup.vars = "day") +
  tm_facets(by = "id", free.coords = TRUE) +
  tmap_options(limits = c(facets.view = 27))

#land.bears.all.ows <- filter(land.bears.all.ows, land.bears.all.ows$id != 'pb_20449.2014') # remove bear with very little data (based on visual inspection)

# Others should possibly be removed, as well, depending on how analysis plays out. Return to this. 

save(land.bears.all.ows, file = 'land_bears_ows.RData')

