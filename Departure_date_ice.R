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
  "pb_20296.2015", "pb_20449.2014", "pb_20713.2004", "pb_20741.2008", "pb_20753.2015", "pb_20794.2006", "pb_20965.2008", "pb_20974.2008", "pb_20982.2008")

insuf <- subset(ice, ice$id %in% insufData)

lat <- ice %>% 
  anti_join(insuf) %>%
  select(id, ymd, gps_lat) 
  

unique(lat$id) # 85 bears

# ----  PLOTTING  ----------------------------------------------------------------------------------------------------------------------- #

# Explore: graph change in latitude 
# x axis = time; y axis = latitude

ex <- ice.df %>% 
  mutate(ordinal = yday(ymd)) %>%
  group_by(id, ordinal) %>%
  summarise(avgLat = mean(gps_lat))

ex$id <- as.character(ex$id)

ggplot(ex, aes(x = ordinal, y = avgLat, col = id)) + 
  geom_line() + 
  facet_wrap(~ id)

# Should fill in missing points - interpolate using CRAWL?

# Classify movement as northward (WNW (315 - ))

# classification: ESE (101.25) - WSW (258.75)  

swim.az$south <- ifelse(swim.az$azimuth > 101.25 & swim.az$azimuth < 258.75, 1, 0)

