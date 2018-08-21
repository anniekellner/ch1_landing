rm(list = ls())
library(rNOMADS)
load("swim_ice.RData")

# example point
lat <- 72.98459
lon<- -159.3032

#Find the latest Global Forecast System model run
model.urls <- GetDODSDates("gfs_0p50")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)