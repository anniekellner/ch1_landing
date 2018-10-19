###################################### 
#####  FD 15 and FD 30 ##############
rm(list = ls())
ice <- read.csv("sbs_daily_icestats_1979_2017_v6.csv")

#Code for changing factor date (01/01/2001) into three columns POSIXct
library(dplyr)
library(lubridate)
ice$date <- as.Date(ice$date, '%m/%d/%Y') # change factor to Date
ice$date <- ymd(ice$date, tz='US/Alaska')

ice <- dplyr::select(ice, date, year, sbs_shelf_icepct_15, sbs_shelf_icepct_30)
colnames(ice) <- c('date', 'year', 'icepct15', 'icepct30')

# Find entries <15 and <30
yr <- 2004:2015
out <- data.frame()

for(i in yr){
  sub <- ice[ice$year==i,]
  date15 <- sub[sub$icepct15 < 15,]
  date30 <- sub[sub$icepct30 < 30,]
  out1 <- cbind(i, date15[1,], date30[1,])
  out <- rbind(out, out1)
}

#refine df
out <- out[-c(3,5,7,8)]
colnames(out) <- c('year', 'FD15', 'icepct15', 'FD30', 'icepct30')
FD <- out

#change FD's to day-of-year
FD$doy15 <- strftime(FD$FD15, format = "%j")
FD$doy15 <- as.numeric(FD$doy15)

FD$doy30 <- strftime(FD$FD30, format = "%j")
FD$doy30 <- as.numeric(FD$doy30)

save(FD, file='FD.RData') #save file



