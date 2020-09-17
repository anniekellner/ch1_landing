##################################################################
#####     Defining Departure Dates for Ice Bears  #################
##################################################################

rm(list = ls())

library(dplyr)
library(tidyr)
library(tmap)

load('all_v2.RData') # Has correct repro info
load('logreg.RData') # Not sure whether has correct repro info 

# ----  Data Prep  ------------------ #

# Remove bears for which there is little data

ice <- all.v2 %>%
  filter(month == 6 | month == 7) %>%
  filter(land_bear == 0) %>%
  select(animal:datetime, id)


noData <- ice %>% # remove bears with < 100 data points
  group_by(id) %>%
  add_count(id) %>%
  filter(n < 50)

ice <- anti_join(ice, noData)

ice.sf <- inner_join(logreg, ice) # include land_bear designation


# Visualize

tmap_mode("view")

tm_shape(ice.sf) + 
  tm_symbols(col = "month", popup.vars = "datetime")  + 
  tm_facets(by = "id") +
  tmap_options(limits = c(facets.view = 100))

# Bears with insufficient data via visual inspection

ice.sf <- ice.sf %>% 
  filter(id != "pb_20794.2005") %>%
  filter(id != "pb_20925.2009") %>%
  filter(id != "pb_21221.2010") %>%
  filter(id != "pb_21283.2012") %>% 
  filter(id != "pb_21291.2012") %>%
  filter(id != "pb_21296.2012") %>%
  filter(id != "pb_21309.2012") %>%
  filter(id != "pb_32799.2006")

unique(ice.sf$id) # 91 ice bears

# Explore: graph change in latitude 

ice.sf$id <- as.factor(ice.sf$id)

ex <- ice.sf %>% 
  filter(id %in% sample(levels(id), 3)) 

# Should fill in missing points - interpolate using CRAWL?

# Classify movement as northward (WNW (315 - ))

# classification: ESE (101.25) - WSW (258.75)  

swim.az$south <- ifelse(swim.az$azimuth > 101.25 & swim.az$azimuth < 258.75, 1, 0)

