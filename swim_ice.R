rm(list = ls())
library(dplyr)

#load data
load("pbears.RData")
ice <- read.csv("sbs_daily_icestats_1979_2017_v4_dcd.csv")

#create swim data frame
start.swim<-filter(all, start.swim==1)
end.swim <- filter(all, end.swim==1)
swim <- rbind(start.swim, end.swim)

#create temp column
swim$tt <- paste(swim$year, swim$month, swim$day)
ice$tt <- paste(ice$year, ice$month, ice$day)

swim.ice <- inner_join(swim,ice, by="tt")
swim.ice<- dplyr::select(swim.ice, animal,year.x,month.x,day.x, hour, minute,second,gps_lat,gps_lon,land,start.swim,end.swim,sbs_shelf_icekm2_15:sbs_meddis_50)
                
save("swim.ice", file="swim_ice.RData")

