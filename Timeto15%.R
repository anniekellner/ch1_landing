#--------------------------------------------------------#
# TIME SINCE 15% SIC AT DEPARTURE LOCATIONS -------------#
#--------------------------------------------------------#

rm(list = ls())

library(dplyr)
library(raster)
library(sf)
library(spData)

# -------------- Prep Data  -----------------------------------------#

#GPS data

load('all_v2.RData')

start <- subset(all.v2, start.swim==1)
start <- start %>% # remove 'undecided'
  filter(!(id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004'))
year <- subset(start, year==2006) #subset by year
year <- dplyr::select(year, id:ymd)

# Create spdf

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs') # epsg projection 3411 NSIDC sea ice polar stereographic north
coords <- cbind(year$X, year$Y)
year.spdf <- SpatialPointsDataFrame(coords = coords, data = year, proj4string = projection) 
year.spdf2 <-spTransform(year.spdf, polar.stereo) #reproject points to polar stereographic



# Create rasterstack
rasterlist <- list.files('./SIC-TIFs/Timeto15%/2006', full.names = TRUE)# bring in all files
st <- stack(rasterlist)

#-------------- Extract values from rasterstack ---------------------------#

loc <- year.spdf2[,c("X", "Y")]
ext <- extract(st, loc, df=TRUE)
SIC <- t(ext)
SIC <- as.data.frame(SIC)

levels(SIC$value) <- c("Date", "SIC")


# bind the extracted values back to the previous dataframe
