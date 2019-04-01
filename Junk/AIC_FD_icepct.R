rm(list = ls())
library(dplyr)

#load data
load("all.RData")
ice <- read.csv("sbs_daily_icestats_1979_2017_v6.csv")

#create swim df
start.swim <- subset(all, start.swim==1)

#create temp column
start.swim$tt <- paste(start.swim$year, start.swim$month, start.swim$day)
ice$tt <- paste(ice$year, ice$month, ice$day)

start.swim.ice <- inner_join(start.swim,ice, by="tt")

#isolate icepct 15, 30, 50
icepct <- dplyr::select(start.swim.ice, animal, datetime, year.y, sbs_shelf_icepct_15, sbs_shelf_icepct_30, sbs_shelf_icepct_50)

# remove September arrival dates
icepct <- icepct[-c(1,3),]

#change datetime to day-of-year
icepct$doy <- strftime(icepct$datetime, format = "%j")
icepct$doy <- as.numeric(icepct$doy)

#change column names
colnames(icepct) <- c('animal', 'datetime', 'year', 'shelf_15', 'shelf_30', 'shelf_50', 'doy.start')

# load FD
load('FD.RData')

#Change dates to day-of-year
FD$doy15 <- strftime(FD$FD15, format="%j")
FD$doy15 <- as.numeric(FD$doy15)

FD$doy30 <- strftime(FD$FD30, format="%j")
FD$doy30 <- as.numeric(FD$doy30)

#join icepct and FD df's
pct.FD <- full_join(icepct, FD, by="year")

# run AIC
library(lme4)

model15 <- lmer(doy.start~shelf_15 + (1|animal), data=pct.FD)
model30 <- lmer(doy.start~shelf_30 + (1|animal), data=pct.FD)
model15.FD <- lmer(doy.start~shelf_15 + doy15 + (1|animal), data=pct.FD)
model30.FD <- lmer(doy.start~shelf_30 + doy30 + (1|animal), data=pct.FD)

library(AICcmodavg)
mods = c(model15, model30, model15.FD, model30.FD)
aictab(mods, second.ord = TRUE)

