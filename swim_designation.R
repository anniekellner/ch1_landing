rm(list = ls())
load('all.RData')
library(dplyr)
swim <- read.csv('swim_092618_v2.csv')

#select relevant columns and reformat times to POSIXct
swim <- select(swim, ID:end.time)
swim$start.datetime <- as.POSIXct(paste(swim$start.date, swim$start.time), tz='US/Alaska')
swim$end.datetime <- as.POSIXct(paste(swim$end.date, swim$end.time), tz='US/Alaska')
all$datetime <- as.POSIXct(all$datetime, tz='US/Alaska')

#create id columns
swim$id <- paste(swim$ID, swim$Year, sep='.')
all$id <- paste(all$animal, all$year, sep='.')

#
all$swim <- 0 
id <- swim$id
for (i in 1:length(id)){
  all$swim[all$datetime <= swim$end.datetime[i] & all$datetime >= swim$start.datetime[i] & all$id==id[i]] <- 1
}

test <- filter(all, id=='pb_20529.2004')
test2 <- filter(test, swim==1)


