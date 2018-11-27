rm(list = ls())

load('all.RData')
start <- subset(all, start.swim==1)
start <- dplyr::select(start, id, datetime, gps_lon, gps_lat)

start$datetime <- as.character(start$datetime) #csv will format posixct weird, so should be as character
start$height.above.ellipsoid<- 10 #from Dave Douglas email

colnames(start) <- c("id", "timestamp", "location-long", "location-lat", "height-above-ellipsoid") #as dictated by movebank

write.table(start, file="C:/Users/akell/Desktop/Fall_2018/Research/swim/movebank3", sep=",", row.names = FALSE) #write to txt bcause csv messes up timestamp
