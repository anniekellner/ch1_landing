rm(list = ls())

library(dplyr)
library(sp)
library(rgdal)
library(ggplot2)
library(lubridate)

#--------------------------------------------#
#--- CALCULATE MIGRATION TIME IN DAYS -------#

load('all_v2.RData')

start <- subset(all.v2, start.swim==1)
start <- start %>% # remove 'undecided'
  filter(!(id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004'))
start <- droplevels(start)

end <- subset(all.v2, end.swim==1)
end <- end %>% # remove 'undecided'
  filter(!(id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004'))
end <- droplevels(end)

mig <- left_join(start, end, by='id')
mig <- select(mig, animal.x, gps_lat.x, gps_lon.x, datetime.x, X.x, Y.x, gps_lat.y, gps_lon.y, X.y, Y.y, datetime.y, ordinal.x)

colnames(mig) <- c('animal', 'lat.start', 'long.start', 'dt.start', 'X.start', 'Y.start', 'lat.end', 'long.end', 'X.end', 'Y.end', 'dt.end', 'ordinal.start')

mig$diff <- difftime(mig$dt.end, mig$dt.start, tz='US/Alaska', units="days")


#------- CALCULATE MIGRATION DISTANCE -------#


swim <- subset(all.v2, swim==1)

dist = swim %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(cum.dist = cumsum(distance))

dist <- as.data.frame(dist)

last <- dist %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice(n())


# ------- LANDING LOCATIONS ---------------#

# Are starting and landing longitudes correlated?

cor(mig$long.start, mig$long.end)
fit <- lm(mig$long.end~mig$long.start) # regression line (y~x)

plot(mig$long.start, mig$long.end, main='Longitudinal correlation between starting and landing', xlab = 'Starting Longitude', ylab = 'Landing Longitude', pch=19)
abline(fit, col='red')
legend('topleft', expression(paste(R^2,'= 0.82, ',italic('P'), '<0.001')))

summary(fit)

# Are landing longitudes uniformly distributed? (support defined as between Point Barrow and US-Canada border)

end <- subset(all.v2, end.swim==1)

hist(end$gps_lon, main="Landing Longitudes", xlab = 'Longitude')

# ----- CHI-SQUARED TEST - ARE DEPARTURE TIMES MORE SIMILAR INTRA-ANNUALLY THAN EXPECTED? -----#

# Null hypothesis: ordinal migration date is independent of year

all.v2$ordinal <- yday(all.v2$ymd)

start <- subset(all.v2, start.swim==1)
start <- start %>% # remove 'undecided'
  filter(!(id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004'))
start <- droplevels(start)

end <- subset(all.v2, end.swim==1)
end <- end %>% # remove 'undecided'
  filter(!(id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004'))
end <- droplevels(end)

mig <- left_join(start, end, by='id')
mig <- select(mig, animal.x, year.x, gps_lat.x, gps_lon.x, datetime.x, X.x, Y.x, gps_lat.y, gps_lon.y, X.y, Y.y, datetime.y, ordinal.x)

colnames(mig) <- c('animal', 'year', 'lat.start', 'long.start', 'dt.start', 'X.start', 'Y.start', 'lat.end', 'long.end', 'X.end', 'Y.end', 'dt.end', 'ordinal.start')

## Gave up on chi-square test because not sure how to classify departure dates categorically #####






#-------- PLOT RESULTS ----------------------#
mig$diff <- as.numeric(mig$diff)

hist(mig$diff, main = 'Migration Time (days)', xlab = 'Days')
  
hist(last$cum.dist, main = 'Migration Distance (km)', xlab = 'Km')

# ---- CREATE SHAPEFILES --------------------#

# Mig Start

start <- select(start, animal:gps_lon, datetime, X,Y)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(start$X, start$Y)
start.spdf <- SpatialPointsDataFrame(coords=coords, data=start, proj4string = projection)
writeOGR(start.spdf, dsn = 'C:/Users/akell/Documents/ArcGIS/Polar_Bears_GIS/Location Data', layer ="start_mig_040419", driver='ESRI Shapefile')

# Mig end
end <- select(end, animal:gps_lon, datetime, X,Y)
projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
coords <- cbind(end$X, end$Y)
end.spdf <- SpatialPointsDataFrame(coords=coords, data=end, proj4string = projection)
writeOGR(end.spdf, dsn = 'C:/Users/akell/Documents/ArcGIS/Polar_Bears_GIS/Location Data', layer ="end_mig_040419", driver='ESRI Shapefile')
