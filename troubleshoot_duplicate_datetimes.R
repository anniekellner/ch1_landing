### TROUBLESHOOTING: duplicate datetimes in traj file ########

rm(list = ls())

library(adehabitatLT)
library(lubridate)

land <- read.csv("ows_all_LAND_v1.csv")
land$datetime2 <- ymd_hms(paste(land$year, land$month, land$day, land$hour, land$minute, land$second, sep = "-"), tz="US/Alaska") #use lubridate fxn to format datetime, add column to land dataframe
traj.pb<-as.ltraj(xy=land[,c("X","Y")], date=land$datetime2, id=as.character(land$animal))

land2<-land[!duplicated(land$datetime2, incomparables = FALSE),] #bc error: non-unique dates for a given burst

## troubleshooting

land$tt<-paste(land$animal, land$datetime2) # putting land and datetime into a single column
land$tt2<-duplicated(land$tt) # add column whether or not entry is duplicated
table(land$tt2) #how many false, how many true

length(tt)
length(unique(tt))

head(land[land$tt2==T,])
tail(land[land$tt2==T,])
table(land[land$tt2==T,"animal"]) #duplicates by animal 




land2 <- read.csv("C:/Users/akell/Desktop/Spring_2018/Research/GIS/Chapter1/Land.csv")
land2$datetime2 <- ymd_hms(paste(land2$year, land2$month, land2$day, land2$hour, land2$minute, land2$second, sep = "-"), tz="US/Alaska") #use lubridate fxn to format datetime, add column to land2 dataframe

traj.land2 <- as.ltraj(xy=land2[,c("X","Y")], date=land2$datetime2, id=as.character(land2$animal))
