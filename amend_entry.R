load('pbears.RData')

all$tt <- paste(all$gps_lat, all$gps_lon, all$animal)  #changing an end.swim entry from 0 to 1 (bc is land on sea ice TIF and is probably land)
all$start.swim[all$tt=='71.3739 -148.8896 pb_20418'] <- 1


all$tt <- paste(all$gps_lat, all$gps_lon, all$animal)
all$end.swim[all$tt=='70.4285 -147.3861 pb_20418'] <- 1

test <- subset(all, end.swim==1)

all$tt <- NULL
save(all, file="all.RData")
