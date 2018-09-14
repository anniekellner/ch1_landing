rm(list = ls())

library(moveHMM)

# prep data
load("all.RData")
pb <- subset(all, animal=='pb_20414' & month==7 & year==2009 )
pb <- dplyr::select(pb, animal, gps_lon,gps_lat)
colnames(pb)[1] <- "ID"

#test
data <- prepData(pb, type="LL", coordNames = c("gps_lon","gps_lat"))
summary(data)

plot(data)
