rm(list = ls())

library(raster)
library(sp)
library(rgdal)

# SIC Analysis: Departure Dates

#create rasterstack using TIFs
rasterlist <- list.files('./SIC TIFs/Departure_Dates', full.names = TRUE)
rasterlist <- stack()







