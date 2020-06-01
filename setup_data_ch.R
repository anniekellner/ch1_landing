##############################################################
#####       Land Bear Database for Survival Model   ##########
##############################################################

rm(list = ls())

library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(tmap)
library(sf)


load('land_bears_ows.RData')

# remove undecided bears

pb <- filter(pb, id != "pb_20413.2006")
pb <- filter(pb, id != "pb_20418.2005")
pb <- filter(pb, id != "pb_20520.2012")
pb <- filter(pb, id != "pb_20529.2004")
pb <- filter(pb, id != "pb_20333.2008")
pb <- filter(pb, id != "pb_21307.2012")
pb <- filter(pb, id != "pb_21307.2014")
pb <- filter(pb, id != "pb_20446.2009")

# visual inspection

tmap_mode("view")

tm_shape(pb) +
  tm_dots(col = "month", size = 1, popup.vars = c("month", "day")) +
  tm_facets(by = "id") +
  tmap_options(limits = c(facets.view = 26))


end.swim <- subset(pb, end.swim == 1) # latest arrival on shore is 9/22

yday("2009-09-22")

mydata <- dplyr::select(pb, id, ymd, land)

mydata$ordinal <- yday(mydata$ymd) # create column for ordinal dates to standardize across years
mydata$on.ice <- ifelse(mydata$land == 0,1,0) # reverse 1 and 0 for model format

mydata <- select(mydata, -land)
mydata <- select(mydata, -ymd)

# Create capture histories
# https://jamesepaterson.github.io/jamespatersonblog/07_creatingcapturehistories

ch <- mydata %>% # if there were both on and off land observations in a single day, took first
  group_by(id, ordinal) %>%
  slice(1)

ch <- distinct(ch)

test <- ch %>%
  pivot_wider(
    names_from = ordinal,
    values_from = on.ice
  )

# In progress - code to combine 1/s and 0/s into single entries
ch <-  %>%
  pivot_wider(ymd, on.ice, fill = 0) %>%
  group_by(id) %>%
  unite("ch", 2:tail(names(.),1), sep = "") 

mydata <- distinct(mydata)



# Example of pivot_wider

warpbreaks <- as_tibble(warpbreaks[c("wool", "tension", "breaks")])
warpbreaks