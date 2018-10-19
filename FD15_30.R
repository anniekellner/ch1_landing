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
  out1 <- c(year, date15[1,], date30[1,]))
  out <- rbind(out, out1)
}