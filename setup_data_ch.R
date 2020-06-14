##########################################################
#######   FORMAT DATA FOR RMARK   ########################
##########################################################

rm(list = ls())

library(RMark)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(tmap)
library(sf)

# -----   LAND BEARS    --------------------------------------------------------------------------- #

load('land_bears_ows.RData')


#data("Blackduck") # example from RMark
#head(Blackduck)

# remove undecided bears

lb <- filter(lb, id != "pb_20413.2006")
lb <- filter(lb, id != "pb_20418.2005")
lb <- filter(lb, id != "pb_20520.2012")
lb <- filter(lb, id != "pb_20529.2004")
lb <- filter(lb, id != "pb_20333.2008")
lb <- filter(lb, id != "pb_21307.2012")
lb <- filter(lb, id != "pb_21307.2014")
lb <- filter(lb, id != "pb_20446.2009")

# visual inspection

#tmap_mode("view")

#tm_shape(lb) +
  #tm_dots(col = "month", size = 1, popup.vars = c("month", "day")) +
  #tm_facets(by = "id") +
 # tmap_options(limits = c(facets.view = 16))


# --  FUNCTIONS  ---------------------------------------------------------------------------- #

prep_data <- function(df) {
    df <- as_tibble(df)
    df$on.ice <- ifelse(df$swim == 1 | df$land == 1, 0, 1)
    df$ordinal <- yday(df$ymd)
    df <- select(df, id, ymd, ordinal, on.ice, land, start.swim)
  return(df)
}

DFtoSF <- function(df) {
  df <- droplevels(df)
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.alb <- st_transform(sf.wgs, albers.proj)
  return(sf.alb)
}

# ------------------------------------------------------------------------------------------- #

# ---   Format capture history (ch) for known-fate models ----------------------- #
# add columns eh1 and eh2
# eh1: 1 = bear was monitored; 0 = bear was censored
# eh2: 1 = bear left ice during following interval; 0 = bear did not die during following interval 

# Chapter 16, Cooch & White 2020

ch <- prep_data(lb)

ss <- subset(ch, start.swim == 1) # 12 bears with start swim date
 
ch$eh1 <- 1 # all recorded observations = 1 (first number in paired ch)

ch <- ch %>% # if there were both on and off land observations in a single day, take last
  group_by(id, ordinal) %>%
  mutate(swim.day = cumsum(start.swim)) %>% # so that swimday = 1
  slice(n()) %>% # select last row
  distinct()
  
 
ss <- subset(ch, swim.day == 1) # all swim days are retained (n = 12)


# Remove bears with < 7 days on land

ch <- ch %>%
  group_by(id) %>%
  arrange(id, ordinal) %>%
  mutate(days.on.land = cumsum(land))

seven.days <- subset(ch, land == 1 & days.on.land > 6) # >= 7 days on land

seven.days.ids <- unique(seven.days$id) # list of bears that spend >=7 days on land
ch <- subset(ch, id %in% seven.days.ids) 

# fill in missing ordinal days with number; eh1 with 0 because animal not observed

ch <- subset(ch, ch$ordinal < 296)


ch <- ch %>%
  group_by(id) %>%
  complete(ordinal = 152:295, fill = list(eh1 = 0)) # 295 = last date of arrival on land

ch[,9][is.na(ch[,9])] <- 0 # change NA's to 0 for ch$eh1


# Encounter history = 1 on occasion BEFORE animal is known to be swimming

ch <- ch %>% 
  group_by(id) %>%
  arrange(id, ordinal) %>%
  mutate(eh2 = if_else(lead(swim.day == 1), 1, 0))

ch[,10][is.na(ch[,10])] <- 0 # change NA's to 0 for ch$eh2

# Change 1's in eh1 to 0's following start.swim

ch <- ch %>%
  group_by(id) %>%
  arrange(id, ordinal) %>%
  mutate(eh1 = replace(eh1, row_number() > which(eh2==1)[1] & eh1 == 1, 0)) 
  
ch$eh <- paste0(ch$eh1, ch$eh2) # create column for encounter history with 10,11,00 pairs


# -------------------------------------------------------------------------------------------------------- #
 
# Create capture histories
# https://jamesepaterson.github.io/jamespatersonblog/07_creatingcapturehistories
     
ch <- dplyr::select(ch, id, ordinal, eh)

ch <- pivot_wider(ch,
    names_from = ordinal,
    values_from = eh)

ch <- ch %>% unite("eh", 2:tail(names(.),1), sep = "")


###################################################################################################

# ----    ICE BEARS  --------------------------------------------------------------------------------- #

load('ice_bears_may2nov.RData')

# visual inspection
# lots of bears with limited data, but going to include preliminarily

library(raster)

albers.proj <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") # Albers Equal Area Conic (same as ArcMap doc)

ice <- DFtoSF(ice)

tmap_mode("view")

tm_shape(ice) +
tm_dots(col = "month", size = 1, popup.vars = c("month", "day")) +
tm_facets(by = "id") +
tmap_options(limits = c(facets.view = 153))

# ----------------------------------------------------------------------------------------------- #

# Remove duplicates from land bears and undecided bears

# Remove land bears


lb.ids <- unique(lb$id) 
t <- subset(ice, id %in% lb.ids) 

ice <- anti_join(ice, t)
ice <- droplevels(ice)

# remove undecided

ice <- filter(ice, id != "pb_20413.2006")
ice <- filter(ice, id != "pb_20418.2005")
ice <- filter(ice, id != "pb_20520.2012")
ice <- filter(ice, id != "pb_20529.2004")
ice <- filter(ice, id != "pb_20333.2008")
ice <- filter(ice, id != "pb_21307.2012")
ice <- filter(ice, id != "pb_21307.2014")
ice <- filter(ice, id != "pb_20446.2009")

# Format for ch 

ice$ordinal <- yday(ice$ymd)
ice <- select(ice, id, ymd, ordinal)  

ch.ice <- ice %>% # take the last observation from each day
  group_by(id, ordinal) %>%
  slice(n()) %>%
  distinct()

ch.ice <- subset(ch.ice, ch.ice$ordinal > 151 & ch.ice$ordinal < 296) 

ch.ice$eh1 <- 1

ch.ice <- ch.ice %>%
  group_by(id) %>%
  complete(ordinal = 152:295, fill = list(eh1 = 0))

ch.ice[,4][is.na(ch.ice[,4])] <- 0 # change NA to 0
ch.ice$eh2 <- 0 # all eh2 is 0

ch.ice$eh <- paste0(ch.ice$eh1, ch.ice$eh2)

# Format ch

ch.ice <- dplyr::select(ch.ice, id, ordinal, eh)

ch.ice <- pivot_wider(ch.ice,
                  names_from = ordinal,
                  values_from = eh)

ch.ice <- ch.ice %>% unite("eh", 2:tail(names(.),1), sep = "")
