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

rl <- dir(path = 'D:/Polar Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/All/RCC/Remaining', pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=FALSE)

head(rl)

# remove elements as necessary
#rl <- rl[-c(1:767)]

rl2 <- str_remove(rl,".tif")

# 12/19/2020: add bears 20525.2014, 20333.2008, 20413.2006, 20418.2005, 20520.2012





# ------------------- Convert rasters to polygons ------------------- #

setwd('D:/Polar Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/All/RCC/Remaining')

bat_pack_ice_poly <- function(rl){
  for (i in 1:length(rl)) {
    r <- raster(paste0("./", rl[i])) #read in raster
    gv <- getValues(r) # change values to vector so can get mode
    mode <- modal(gv, na.rm=TRUE) # find mode
    poly <- rasterToPolygons(r, function(x){x==mode}, dissolve = TRUE) #raster to polygon
    poly <- spTransform(poly, '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')
    writeOGR(poly, dsn = 'D:/Polar Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/POLY', layer = rl[i], driver = 'ESRI Shapefile')
    }
  }
  
#run the function
bat_pack_ice_poly(rl)


# -------------------------------------------------------------------------------------------------------- #
# Test the script - make sure the polygon looks like the raster. 
#Pairwise assessment to make sure R1-R2 are different; R1-P1 similar; R2-P2 similar


r1 <- raster('D:/Polar Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/All/RCC/Remaining/asi-n3125-20090601-v5.4.tif')
p1 <- st_read("D:/Polar Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/POLY2/asi-n3125-20090601-v5.4.tif.shp")

plot(r1)
plot(st_geometry(p1))







