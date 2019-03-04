rm(list = ls())
library(dplyr)

load('all.RData')

swim <- subset(all, swim==1)

# filter out undecided

swim <- filter(swim, id != "pb_20413.2006")
swim <- filter(swim, id != "pb_20418.2005")
swim <- filter(swim, id != "pb_20520.2012")
swim <- filter(swim, id != "pb_20529.2004")
swim <- filter(swim, id != "pb_20333.2008")
