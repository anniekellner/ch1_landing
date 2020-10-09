#############################################
### MOVEBANK ######################
#############################################

rm(list = ls())

library(lubridate)
library(dplyr)
library(sf)

load('land_bears_CoxPH.RData') 
new <- subset(bears, id == "pb_20333.2008" | id == "pb_20525.2014")
new <- distinct(new)

# select columns

movebank <- dplyr::select(new, datetime, gps_lon, gps_lat, id)
head(movebank)

colnames(movebank) <- c('timestamp', 'location-long', 'location-lat', 'id')

tz(movebank$timestamp) # should be US/Alaska

movebank$timestamp <- with_tz(movebank$timestamp, "UTC") # Change date and time to UTC
#movebank$timestamp <- paste0(movebank$timestamp, ".00") # required movebank formatting

movebank$ht.above.ellipsoid <-0 #need to change '.' to '-' in .csv

movebank <- movebank[, c(1:3, 5,4)] # reorder columns to fit Movebank format
head(movebank)

movebank$timestamp <- ymd_hms(movebank$timestamp)


write.csv(movebank, 'C:/Users/akell/Documents/PhD/Polar_Bears/Data/Movebank_10082020.csv', row.names = FALSE)
