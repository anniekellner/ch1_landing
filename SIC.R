rm(list = ls())

library(raster)
library(sp)
library(rgdal)

# test:import single file 
Jul282012 <- "asi-AMSR2-n6250-20120728-v5.tif"
Jul282012.ras <- raster() # this works
extent(Jul282012.ras) #extent is entire globe - should be same for all

#import files one-by-one
Aug82013 <- "asi-AMSR2-n6250-20130815-v5.tif"
Aug82013.ras <- raster()
extent(Aug82013.ras)

Jul192009 <- 'asi-n6250-20090719-v5.4.tif'
Jul192009.ras <- raster()
extent(Jul192009.ras) #v5 and v5.4 have same extent


#######################################################################
# test: import multiple .tif files at once --> FAIL (different extents)
rasterlist <- list.files('./SIC TIFs/Departure_Dates', full.names = TRUE)
rasterlist <- stack()

trial2 <- list.files('C:/Users/akell/Desktop/Fall_2018/Research/swim/Sea Ice TIFs/Departure_Dates/test')
stacktry2 <- stack(trial2)

trialstack <- stack()

