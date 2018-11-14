rm(list = ls())

load('all.RData')
start <- subset(all, start.swim==1)
start <- dplyr::select(start, id, datetime, gps_lon, gps_lat)
start$datetime <- as.character(start$datetime)
start$height.above.ellipsoid<- 10



write.csv(start, file='C:/Users/akell/Desktop/Fall_2018/Research/swim/start_movebank.csv', row.names = F)
