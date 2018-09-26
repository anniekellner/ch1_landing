rm(list = ls())

library(dplyr)
load('all.RData')

all$temp <- paste(all$animal, all$year) # ID by animal and year

start <- filter(all, start.swim==1)
start <- select(start, temp, gps_lon, gps_lat)

end <- filter(all, end.swim==1)
end <- select(end, temp, gps_lon, gps_lat)

colnames(start) <- c("ID", "start.lon", "start.lat")
colnames(end) <- c("ID", "end.lon", "end.lat")

cor <- full_join(start, end, by="ID")

cor(cor$start.lon, cor$end.lon)
cor(cor$start.lat, cor$end.lat)

cor.test(cor$start.lon, cor$end.lon) #test significance
fit <- lm(cor$end.lon ~ cor$start.lon)
summary(fit)

plot(cor$start.lon, cor$end.lon, main="Longitudinal correlation between starting and landing points of polar bear summer migration to land", xlab = 'Starting Longitude', ylab = 'Landing Longitude', pch=19)
abline(fit, col='red')
legend('topleft', expression(paste(r,'=0.815, ',italic('P'), '<0.001')))
