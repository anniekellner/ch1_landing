rm(list = ls())

library(dplyr)
library(lubridate)

## Prep database from TA
all <- read.csv("usgs_pbear_gps_ccde16_v20170131.csv")
all <- dplyr::select(all, animal:date) #another loaded package probably has a 'select' fxn, so specify dplyr
all$datetime <- as.character(ymd_hms(paste(all$year, all$month, all$day, all$hour, all$minute, all$second, sep = "-"), tz="US/Alaska"))
all <- na.omit(all) # delete datetimes that failed to parse

### land ###
land <- read.csv("Land.csv")
land<-dplyr::select(land, animal:date, datetime)
land <- na.omit(land)
land$tt<-paste(land$animal, land$datetime) 
all$id.datetime<-paste(all$animal, all$datetime)
tt2<-land$tt
all$land<-ifelse(all$id.datetime %in% tt2, 1, 0)

all$tt <- paste(all$gps_lat, all$gps_lon)  #changing an end.swim entry from 0 to 1 (bc is land on sea ice TIF and is probably land)
all$land[all$tt=='70.5425 -150.7114'] <- 1

#### swim ###

swim <- read.csv("swim.csv")
swim<-dplyr::select(swim, ID:end.time)
swim <- na.omit(swim)
swim$start.datetime <- as.character(ymd_hms(paste(swim$start.date, swim$start.time))) #create column for datetime
swim$end.datetime <- as.character(ymd_hms(paste(swim$end.date, swim$end.time)))

swim$tt.start <- paste(swim$ID, swim$start.datetime) #temp start
swim$tt.end <- paste(swim$ID, swim$end.datetime) #temp end
tt.start <- swim$tt.start
tt.end <- swim$tt.end
all$start.swim <- ifelse(all$id.datetime %in% tt.start, 1,0)
all$end.swim <- ifelse(all$id.datetime %in% tt.end, 1,0)

## save df as .RData
all<-dplyr::select(all, -tt)
all<- dplyr::select(all, -id.datetime)
save("all", file="pbears.RData")

## Checking ####

library(lubridate)
test<-filter(all, start.swim==1)
test2 <- filter(all, land==1)
test3 <- filter(all, end.swim==1)

###########################
### JUNK #################

swim$Start.Date <- as.character((swim$Start.Date)) #Didn't work
strptime(swim$Start.Date, "%m-%d-%Y %H:%M:%S") #Didn't work
as.POSIXct(swim$Start.Date, tz="US/Alaska", sep="-") #Didn't work

filter(all, animal=="pb_20845"& year==2015 & month==8 & day==27) #To check datetimes, because seconds (:ss) reset to 0 in Excel

#resolving differences in land and test2 --> there are 6 different. Not sure why but not in OWS anyway. 
test2.comp <- dplyr::select(test2, animal:gps_lon)
land.comp<- dplyr::select(land, animal:gps_lon)
dplyr::setdiff(land.comp, test2.comp)

land<-dplyr::distinct(land) #no duplicates
test2 <- dplyr::distinct(test2) #no duplicates
