######################################################
###           BATCH RASTER TO POLYGON       ##########
######################################################

rm(list = ls())

library(raster)
library(rgdal)
library(sf)


# ----------- load data ----------------------------------------------#

load('ded_ids.RData')

ded <- ded[-c(1:3)] # remove folder names that already have polygons

rl <- dir(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/RCC", pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=FALSE)

# remove elements as necessary

rl <- rl[-c(1:127)]
rl2 <- str_remove(rl,".tif")

rl3 <- character()
for(i in 1:length(rl2)){
  rl3[i] <- paste0(rl2[i], "/polygon")
}

 # paste0 collapses the string so there is no extra whitespace

#Create directories for each shapefile
for(i in 1:length(rl2)){
dir.create(paste0('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/POLY/', rl2[i]))
}

# ------------------- Convert rasters to polygons ------------------- #

bat_pack_ice_poly <- function(rl){
  for (i in 1:length(rl)) {
    for(j in 1:length(rl3)){
    r <- raster(paste0("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/RCC/", rl[i])) #read in raster
    gv <- getValues(r) # change values to vector so can get mode
    mode <- modal(gv, na.rm=TRUE) # find mode
    poly <- rasterToPolygons(r, function(x){x==mode}, dissolve = TRUE) #raster to polygon
    spTransform(poly, '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')
    writeOGR(poly, dsn = 'C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/POLY', layer = rl3[j], driver = 'ESRI Shapefile')
    }
  }
  }

#run the function
bat_pack_ice_poly(rl)



