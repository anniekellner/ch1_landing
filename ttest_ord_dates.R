rm(list = ls())

load('all.RData')
start <- subset(all, start.swim==1)

# create column for denning bears

start$den <- ifelse(start$id=='pb_20413.2006' | start$id=='pb_20735.2009' | start$id=='pb_20414.2009' | start$id=='pb_20446.2009' | start$id=='pb_21264.2011' | start$id=='pb_32366.2011' | start$id=='pb_20520.2012' | start$id=='pb_21358.2013' | start$id=='pb_21015.2013' | start$id=='pb_21368.2014', 1, 0)

#Change datetime to day-of-year

library(lubridate)
start$ymd <- ymd(paste(start$year, start$month, start$day))
start$doy <- strftime(start$ymd, format='%j')
start$doy <- as.numeric(start$doy)

## t-test

# create datasets to contrast

den <- subset(start, den==1)
no.den <- subset(start, den==0)
no.den[-8,] # remove pb_20845 because den=NA

t.test(den$doy, no.den$doy)

