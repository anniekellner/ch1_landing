library(dplyr)
library(adehabitatLT)
library(lubridate)

land <- read.csv("ows_land_v2.csv")
land$datetime <- as.POSIXct(land$datetime, tz = "US/Alaska") #change datetime to POSIXct to be read by adehabitatLT


traj.pb<-as.ltraj(xy=land[,c("X","Y")], date=land$datetime, id=as.character(land$animal))
Sum.traj <- summary(traj.pb)

all <- read.csv("usgs_pbear_gps_ccde16_v20170131.csv")
all <- select(all, animal:date)

#add datetime column
all$datetime <- ymd_hms(paste(all$year, all$month, all$day, all$hour, all$minute, all$second, sep = "-"), tz="US/Alaska")




