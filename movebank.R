#############################################
### MOVEBANK ######################
#############################################

rm(list = ls())

library(lubridate)
library(dplyr)
library(sf)

load('logreg.RData') 

# add lat long data

logreg.wgs <- st_transform(logreg, crs = 4326)
logreg.wgs <- cbind(logreg.wgs, st_coordinates(logreg.wgs$geometry))

movebank <- select(logreg.wgs, id, datetime, X.1, Y.1)
movebank <- as.data.frame(movebank)
movebank <- movebank %>% select(-geometry)

colnames(movebank) <- c('id', 'timestamp', 'location-long', 'location-lat')

tz(movebank$timestamp) # should be US/Alaska

movebank$timestamp <- with_tz(movebank$timestamp, "UTC") # Change date and time to UTC

movebank$ht.above.ellipsoid <-0 #need to change '.' to '-' in .csv

movebank2 <- movebank[, c(2,3,4,5,1)] # reorder columns to fit Movebank format

write.csv(movebank2, 'C:/Users/akell/Documents/PhD/Polar_Bears/Data/Movebank_07032020.csv', row.names = FALSE)
