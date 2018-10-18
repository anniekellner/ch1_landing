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

#change datetime to day-of-year
icepct$doy <- strftime(icepct$datetime, format = "%j")
icepct$doy <- as.numeric(icepct$doy)

#change column names
colnames(icepct) <- c('animal', 'datetime', 'year', 'shelf_15', 'shelf_30', 'shelf_50', 'doy')

# linear mixed models
library(lme4)
model15 <- lmer(doy~shelf_15 + year +(1|animal), data=icepct)
model30 <- lmer(doy~shelf_30 + year +(1|animal), data=icepct)
model50 <- lmer(doy~shelf_50 + year +(1|animal), data=icepct)

AIC(model15, model30, model50)

# remove September arrival dates
icepct.no9 <- icepct[-c(1,3),]

#re-do AIC without September dates
model15 <- lmer(doy~shelf_15 + year +(1|animal), data=icepct.no9)
model30 <- lmer(doy~shelf_30 + year +(1|animal), data=icepct.no9)
model50 <- lmer(doy~shelf_50 + year +(1|animal), data=icepct.no9)

AIC()
