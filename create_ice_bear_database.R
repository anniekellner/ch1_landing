########################################################################
######      Ice Bears Dataset   ########################################
########################################################################

rm(list = ls())

library(dplyr)
library(tidyselect)

# load data

load('all_v2.RData')

# projection

polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') 

# Function to turn df into sf (cannot droplevels in sf object so always need to start w df)

DFtoSF <- function(df) {
  df <- droplevels(df)
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.ps <- st_transform(sf.wgs, polar.stereo)
  return(sf.ps)
}

# remove ice bears with insufficient data

ice <- subset(all.v2, month > 4 & month < 12 & land_bear_ows == 0)

noData <- ice %>% # remove bears with < 100 data points
  add_count(id) %>%
  filter(n < 100)

ice <- anti_join(ice, noData) # 133 bears

rem <- c("pb_20926.2015", "pb_20474.2009", "pb_20492.2008", "pb_20694.2004", "pb_21402.2016", "pb_21403.2016", "pb_22299.2015", "pb_22305.2015","pb_22318.2016", "pb_32362.2015", "pb_32608.2008", "pb_32921.2016")

ice <- ice %>% # these were from visual inspection of trajectories
  filter(!(id %in% rem))

save(ice, file = 'ice_bears_may2nov.RData')

### ----- Data Exploration  ------------------------------------------------------------------------------------------- ###

# how many fixes per day?

fix <- ice %>%
  group_by(id, ymd) %>%
  arrange(id, ymd) %>%
  summarise(n())

mean(fix$`n()`) # mean daily fix = 9.34



