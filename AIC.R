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
colnames(icepct) <- c('animal', 'datetime', 'year', 'shelf_15', 'shelf_30', 'shelf_50', 'doy.start')

# linear mixed models - sea ice concentration only 
library(lme4)
model15.yr <- lmer(doy~shelf_15 + year +(1|animal), data=icepct)
model30.yr <- lmer(doy~shelf_30 + year +(1|animal), data=icepct)
model50.yr <- lmer(doy~shelf_50 + year +(1|animal), data=icepct)

AIC(model15, model30, model50)

# remove September arrival dates
icepct.no9 <- icepct[-c(1,3),]

#re-do AIC without September dates
model15.yr <- lmer(doy~shelf_15 + year +(1|animal), data=icepct.no9)
model30.yr <- lmer(doy~shelf_30 + year +(1|animal), data=icepct.no9)
model50.yr <- lmer(doy~shelf_50 + year +(1|animal), data=icepct.no9)
model15 <- lmer(doy~shelf_15 + (1|animal), data=icepct.no9)
model30 <- lmer(doy~shelf_30 + (1|animal), data=icepct.no9)
model50 <- lmer(doy~shelf_50 + (1|animal), data=icepct.no9)


#calculate aicc - sea ice concentration only 
library(AICcmodavg)
mods = c(model15.yr, model30.yr, model50.yr, model15, model30, model50)
aictab(mods, second.ord = TRUE)
