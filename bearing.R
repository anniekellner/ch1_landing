rm(list = ls())

load('all.RData')
swim <- subset(all, swim==1)

library(adehabitatLT)
library(proj4)

# project lat/long to grid for adehabitat
M <- as.matrix(cbind(swim$gps_lon, swim$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]
swim2 <- cbind(swim,X,Y) #bind the datasets

#to POSIXct
swim2$datetime <- as.POSIXct(swim2$datetime, tz='US/Alaska')


#Divide into groups with same fix interval
one <- subset(swim2, id=='pb_20414.2009' | id=='pb_20446.2009' | id=='pb_20735.2009' | id=='pb_20845.2015')
two <- subset(swim2, id=='pb_20520.2012' | id=='pb_20525.2013' | id=='pb_20525.2014' | id=='pb_20529.2004' | id=='pb_20529.2005'| id=='pb_21015.2013' | id=='pb_21358.2013' | id=='pb_21368.2014' | id=='pb_32366.2014')
four <- subset(swim2, id=='pb_20333.2008' | id=='pb_21264.2011' | id=='pb_32366.2011')
eight <- subset(swim2, id=='pb_06817.2006' | id=='pb_20413.2006' | id=='pb_20418.2005')


#convert to traj 
swim1.traj <- as.ltraj(xy=one[,c("X","Y")], date=one$datetime, id=one$id, burst=one$id, proj4string = CRS(("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")))
swim2.traj <- as.ltraj(xy=two[,c("X","Y")], date=two$datetime, id=two$id, burst=two$id, proj4string = CRS(("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")))
swim4.traj <- as.ltraj(xy=four[,c("X","Y")], date=four$datetime, id=four$id, burst=four$id, proj4string = CRS(("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")))
swim8.traj <- as.ltraj(xy=eight[,c("X","Y")], date=eight$datetime, id=eight$id, burst=eight$id, proj4string = CRS(("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")))

#create regular trajectories
library(lubridate)
refda <- parse_date_time(paste(min(swim2$datetime)), orders = 'ymd HMS', tz = 'US/Alaska') #set refda
swim1.traj.NA <- setNA(swim1.traj, refda, 1, units = 'hour') #setNA
swim1.traj.NA0 <- sett0(swim1.traj.NA, refda, 1, units = 'hour') #round times to create regular trajectory

swim1.df <- ld(swim1.traj.NA)

## Convert t1 to LL
library(proj4)
M <- as.matrix(cbind(swim1.df$x, swim1.df$y)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", inverse=TRUE) #project to lat/long
long.t1 <- xy[,1]
lat.t1 <- xy[,2]

swim1.df <- cbind(swim1.df,long.t1,lat.t1) #bind the datasets


## Create column for difference (t2x = location x when t = x+1)
## t2 = t + 1

library(dplyr)
swim1.df<- dplyr::mutate(swim1.df, t2x=x+dx)
swim1.df <- dplyr::mutate(swim1.df, t2y=y+dy)

#convert t2 back to lat/long

library(proj4)
M <- as.matrix(cbind(swim1.df$t2x, swim1.df$t2y)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", inverse=TRUE) #project to lat/long
long.t2 <- xy[,1]
lat.t2 <- xy[,2]

swim1.df <- cbind(swim1.df,long.t2,lat.t2) #bind the datasets

swim1.df$LL.t1 <- c(swim1.df$long.t1, swim1.df$lat.t1)
swim1.df$LL.t2 <- paste(swim1.df$long.t2, swim1.df$lat.t2, sep=",")


## find bearing

library(geosphere)
#bearing(c(swim1.df$long.t1[1], swim1.df$lat.t1[1]), c(swim1.df$long.t2[1], swim1.df$lat.t2[1])) #test - works. Verified using azimuth in TA database


