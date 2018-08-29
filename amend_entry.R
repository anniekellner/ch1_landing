load('pbears.RData')

all$tt <- paste(all$gps_lat, all$gps_lon)  #changing an end.swim entry from 0 to 1 (bc is land on sea ice TIF and is probably land)
all$start.swim[all$tt=='71.1208 -151.0584'] <- 1


all$tt <- paste(all$gps_lat, all$gps_lon, all$day)
all$end.swim[all$tt=='70.4979 -149.6808 17'] <- 1

test <- subset(all, end.swim==1)
