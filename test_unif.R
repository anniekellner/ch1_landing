rm(list = ls())

load('all.RData')

start <- subset(all, start.swim==1)
end <- subset(all, end.swim==1)

end.long <- end$gps_lon
hist(end.long, main="Landing Longitudes", xlab = 'Longitude')


ks.test(end.long, "punif", 141.00, 156.46) # punif takes into account entire CDF, better than creating a dummy distribution to compare (e.g.runif) 



