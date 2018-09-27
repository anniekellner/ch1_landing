rm(list = ls())
library(spgs)

load('all.RData')

start <- subset(all, start.swim==1)
end <- subset(all, end.swim==1)

end.long <- end$gps_lon
hist(end.long, main="Landing Longitudes", xlab = 'Longitude')

