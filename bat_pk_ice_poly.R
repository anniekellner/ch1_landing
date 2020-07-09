######################################################
###           BATCH RASTER TO POLYGON       ##########
######################################################

rm(list = ls())

library(raster)
library(rgdal)
library(sf)
library(stringr)


# ----------- load data ----------------------------------------------#

#load('ded_ids.RData')
#ded <- ded[-c(1:3)] # remove folder names that already have polygons

rl <- dir(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/All/RCC", pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=FALSE)

# remove elements as necessary
#rl <- rl[-c(1:127)]

rl2 <- str_remove(rl,".tif")

rl3 <- character()
for(i in 1:length(rl2)){
  rl3[i] <- paste0(rl2[i], "/polygon")
}

 # paste0 collapses the string so there is no extra whitespace

#Create directories for each shapefile
for(i in 1:length(rl2)){
dir.create(paste0('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/POLY/', rl2[i]))
}

# ------------------- Convert rasters to polygons ------------------- #

bat_pack_ice_poly <- function(rl, rl2){
  for (i in 1:length(rl)) {
    r <- raster(paste0("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/All/RCC/", rl[i])) #read in raster
    gv <- getValues(r) # change values to vector so can get mode
    mode <- modal(gv, na.rm=TRUE) # find mode
    poly <- rasterToPolygons(r, function(x){x==mode}, dissolve = TRUE) #raster to polygon
    poly <- spTransform(poly, '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')
    writeOGR(poly, dsn = 'C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/POLY', layer = rl2[i], driver = 'ESRI Shapefile')
    }
  }
  

#run the function
bat_pack_ice_poly(rl, rl2)

# -------------------------------------------------------------------------------------------------------- #
# Test the script - make sure the polygon looks like the raster. 
#Pairwise assessment to make sure R1-R2 are different; R1-P1 similar; R2-P2 similar


setwd('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen')


r1 <- raster('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/RCC/pb_20525.2013/asi-AMSR2-n6250-20130717-v5.4.tif')
p1 <- st_read('./POLY/pb_20525.2013/asi-AMSR2-n6250-20130719-v5.4/polygon.shp')

plot(r1)
plot(st_geometry(p1))

r2 <- raster('./RCC/pb_20529.2005/asi-n6250-20050820-v5.4.tif')
p2 <- st_read('./POLY/pb_20529.2005/asi-n6250-20050820-v5.4/polygon.shp')

plot(r2)
plot(st_geometry(p2))

# ------------------------------------------------------------------------------------------- #

# BEcause I needed stopped running the third folder mid-run in 2019

library(stringr)

rl <- dir(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/RCC/pb_20525.2013", pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=FALSE)
rl <- rl[-c(3:32)]
rl2 <- str_remove(rl,".tif")

rl3 <- character()
for(i in 1:length(rl2)){
  rl3[i] <- paste0(rl2[i], "/polygon")
}


#Create directories for each shapefile
for(i in 1:length(rl2)){
  dir.create(paste0('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/POLY/', rl2[i]))
}


for (i in 1:length(rl)) {
  for(j in 1:length(rl3)){
    r <- raster(paste0("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/RCC/pb_20525.2013/", rl[i])) #read in raster
    gv <- getValues(r) # change values to vector so can get mode
    mode <- modal(gv, na.rm=TRUE) # find mode
    poly <- rasterToPolygons(r, function(x){x==mode}, dissolve = TRUE) #raster to polygon
    spTransform(poly, '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')
    writeOGR(poly, dsn = 'C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/POLY', layer = rl3[j], driver = 'ESRI Shapefile')
  }
}

