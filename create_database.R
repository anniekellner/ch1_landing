rm(list = ls())

library(dplyr)
library(adehabitatLT)
library(lubridate)

all <- read.csv("usgs_pbear_gps_ccde16_v20170131.csv")
all <- dplyr::select(all, animal:date) #another loaded package probably has a 'select' fxn, so specify dplyr
land <- read.csv("Land.csv")
land<-dplyr::select(land, animal:date, datetime)

#make sure dataframes have identical columns
all$datetime <- ymd_hms(paste(all$year, all$month, all$day, all$hour, all$minute, all$second, sep = "-"), tz="US/Alaska")
land$distance<- land$distanc
land$data_origin <- land$dat_rgn

#paste into common column
land$tt<-paste(land$animal, land$datetime) #land
all$tt<-paste(all$animal, all$datetime)

tt2<-land$tt

all$land<-ifelse(all$tt %in% tt2, 1, 0)

swim <- read.csv("Landing.csv") #swim
swim <- na.omit(swim) #remove NAs
swim<-dplyr::select(swim, ID:End.long) #eliminate unnecessary columns

#Format datetime
swim$Start.Date <- mdy_hms(swim$Start.Date)
swim$Start.Date <- as.character(swim$Start.Date)
swim$End.Date <- mdy_hms(swim$End.Date)
swim$End.Date<- as.character(swim$End.Date)

swim$tt <- paste(swim$ID, swim$Start.Date) 
tt3 <- swim$tt
all$start.swim <- ifelse(all$tt %in% tt3, 1,0)

swim$tt.end <- paste(swim$ID, swim$End.Date) #end swim
tt4 <- swim$tt.end
all$end.swim <- ifelse(all$tt %in% tt4,1,0)

write.csv(all, "data080918.csv")


class(all$year)

###########################
### JUNK #################

swim$Start.Date <- as.character((swim$Start.Date)) #Didn't work
strptime(swim$Start.Date, "%m-%d-%Y %H:%M:%S") #Didn't work
as.POSIXct(swim$Start.Date, tz="US/Alaska", sep="-") #Didn't work

filter(all, animal=="pb_20845"& year==2015 & month==8 & day==27) #To check datetimes, because seconds (:ss) reset to 0 in Excel
