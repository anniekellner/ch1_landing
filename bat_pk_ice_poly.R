######################################################
###           BATCH RASTER TO POLYGON       ##########
######################################################

rm(list = ls())

library(raster)
library(sf)


# ----------- load data ----------------------------------------------#

load('ded_ids.RData')

rl <- dir(path = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/RCC", pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=FALSE)

# remove elements as necessary
as.list(rl)
rl <- rl[-c(1:97)]

as.character(rl)

# Create directories for new polygons

#for(i in 1:length(ded)){
#dir.create(paste0('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/POLY/', ded[i]))
#}

# ------------------- Convert rasters to polygons ------------------- #

bat_pack_ice_poly <- function(rl){
  for (i in 1:length(rl)) {
    r <- raster(paste0("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/RCC/", rl[i])) #read in raster
    gv <- getValues(r) # change values to vector so can get mode
    mode <- modal(gv, na.rm=TRUE) # find mode
    poly <- rasterToPolygons(r, function(x){x==mode}, dissolve = TRUE) #raster to polygon
     #convert to sf object
    poly <- st_transform(poly, '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')
    st_write(poly, paste0('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/POLY/', rl[1]), driver='ESRI shapefile', update=TRUE, quiet = TRUE)}}

#run the function
bat_pack_ice_poly(rl)
################################################################
##    TROUBLESHOOTING

r <- raster(paste0("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/RCC/", rl[1]))
gv <- getValues(r) # change values to vector so can get mode
mode <- modal(gv, na.rm=TRUE) # find mode
poly <- rasterToPolygons(r, function(x){x==mode}, dissolve = TRUE)
writeOGR(poly, dsn = "C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/POLY", layer = rl[1], driver = 'ESRI Shapefile')


r2 <- gsub(".", "-", r@data@names, fixed = TRUE)
r2 <- sub("5-4", "5.4", r2)
r@data@names <- r2

poly <- st_transform(poly, '+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0')
writeOGR(spdf.use,dsn = "C:/Users/akell/Documents/ArcGIS/Polar_Bears_GIS/Location Data", layer = "start_migration", driver = "ESRI Shapefile")

st_write(poly, paste0('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/POLY/', rl[1]), driver='ESRI shapefile', delete_layer = TRUE)