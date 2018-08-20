rm(list = ls())

library(dplyr)
library(adehabitatLT)
library(lubridate)

## Prep database from TA
all <- read.csv("usgs_pbear_gps_ccde16_v20170131.csv")
all <- dplyr::select(all, animal:date) #another loaded package probably has a 'select' fxn, so specify dplyr
all$datetime <- as.character(ymd_hms(paste(all$year, all$month, all$day, all$hour, all$minute, all$second, sep = "-"), tz="US/Alaska"))
all <- na.omit(all) # delete datetimes that failed to parse

### land ###
land <- read.csv("Land.csv")
land<-dplyr::select(land, animal:date, datetime)
land$tt<-paste(land$animal, land$datetime) 
all$id.datetime<-paste(all$animal, all$datetime)
tt2<-land$tt
all$land<-ifelse(all$id.datetime %in% tt2, 1, 0)

#### swim ###

swim <- read.csv("swim.csv")
swim<-dplyr::select(swim, ID:end.time)
swim <- na.omit(swim)

swim$tt.start <- paste(swim$ID, swim$start.datetime) #temp start
swim$tt.end <- paste(swim$ID, swim$end.datetime) #temp end
tt.start <- swim$tt.start
tt.end <- swim$tt.end
all$start.swim <- ifelse(all$id.datetime %in% tt.start, 1,0)
all$end.swim <- ifelse(all$id.datetime %in% tt.start, 1,0)

## save df as .Rda
save("all", file="mydata.RData")


###########################
### JUNK #################

swim$Start.Date <- as.character((swim$Start.Date)) #Didn't work
strptime(swim$Start.Date, "%m-%d-%Y %H:%M:%S") #Didn't work
as.POSIXct(swim$Start.Date, tz="US/Alaska", sep="-") #Didn't work

filter(all, animal=="pb_20845"& year==2015 & month==8 & day==27) #To check datetimes, because seconds (:ss) reset to 0 in Excel
