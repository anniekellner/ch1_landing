#############################################
### MOVEBANK ######################
#############################################

rm(list = ls())
library(lubridate)
library(dplyr)

load('Ice_Measurements.RData') #pb_06817 only 3/30/19

movebank <- select(df.pb, id, datetime, gps_lon, gps_lat)

colnames(movebank) <- c('id', 'timestamp', 'location-long', 'location-lat')

movebank$timestamp <- with_tz(movebank$timestamp, "UTC") # Change date and time to UTC

movebank$ht.above.ellipsoid <-0 #need to change '.' to '-' in .csv


write.csv(movebank, 'C:/Users/akell/Desktop/Spring 2019/Research/Chapter1/Windtest_movebank_06817.csv')
