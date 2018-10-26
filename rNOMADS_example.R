rm(list = ls())
library(rNOMADS)
library(dplyr)

load("all.RData")
swim <- subset(all, swim==1)
swim <- arrange(swim, year)

# Change date and time to UTC
library(lubridate)
swim$UTC <- with_tz(swim$datetime, "UTC")

# example point
lat <- 72.98459
lon<- -159.3032

#Find the latest Global Forecast System model run
model.urls <- GetDODSDates("gfsanl")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)