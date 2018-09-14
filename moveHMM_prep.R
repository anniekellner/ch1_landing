rm(list = ls())
library(dplyr)

load('all.RData')

# subset by swims, using data from month(s) in which hypothesized swims occur
pb1 <- subset(all, animal=='pb_06817' & month==9 & year==2006)
pb2 <- subset(all, animal=='pb_20333' & month==8 & year==2008)
pb3 <- subset(all, animal=='pb_20413' & month==9 & year==2006)
pb4 <- subset(all, animal=='pb_20414' & month==7 & year==2009)
pb5 <- subset(all, animal=='pb_20418' & month==7|month==8 & year==2005)
pb6 <- subset(all, animal=='pb_20446' & month==7 & year==2009)
pb7 <- subset(all, animal=='pb_20520' & month==8 & year==2012)
pb8 <- subset(all, animal=='pb_20525' & month==8 & year==2013)
pb9 <- subset(all, animal=='pb_20525' & month==8 & year==2014)
pb10 <- subset(all, animal=='pb_20529' & month==8 & year==2004)
pb11 <- subset(all, animal=='pb_20529' & month==9 & year==2005)
pb12 <- subset(all, animal=='pb_20735' & month==7 & year==2009)
pb13 <- subset(all, animal=='pb_20845' & month==8 & year==2015)
pb14 <- subset(all, animal=='pb_21015' & month==8 & year==2013)
pb15 <- subset(all, animal=='pb_21264' & month==7 | month==8 & year==2011)
pb16 <- subset(all, animal=='pb_21358' & month==8 & year==2013)
pb17 <- subset(all, animal=='pb_21368' & month==8 & year==2014)
pb18 <- subset(all, animal=='pb_32366' & month==8 & year==2011)
pb19 <- subset(all, animal=='pb_32366' & month==8 & year==2014)

swimHMM <- rbind(pb1, pb2, pb3, pb4, pb5, pb6, pb7, pb8, pb9, pb10, pb11, pb12, pb13, pb14, pb15, pb16, pb17, pb18, pb19)

# add ice data
ice <- read.csv("sbs_daily_icestats_1979_2017_v4_dcd.csv")

swimHMM$tt <- paste(swimHMM$year, swimHMM$month, swimHMM$day)
ice$tt <- paste(ice$year, ice$month, ice$day)

swimHMM.ice <- inner_join(swimHMM,ice, by="tt")


