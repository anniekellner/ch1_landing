load('all.RData')

all$tt <- paste(all$gps_lat, all$gps_lon, all$animal)  #changing an end.swim entry from 0 to 1 (bc is land on sea ice TIF and is probably land)
all$start.swim[all$tt=='71.3739 -148.8896 pb_20418'] <- 1


all$tt <- paste(all$animal, all$month, all$day, all$hour)
all$start.swim[all$tt=='pb_20845 8 27 11'] <- 0

all$start.swim[all$animal=='pb_20520'] <- 0
all$end.swim[all$animal=='pb_20520'] <- 0


test <- subset(all, end.swim==1)

all$tt <- NULL
save(all, file="all.RData")


