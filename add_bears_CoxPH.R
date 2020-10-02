#########################################################################
####      ADD BEARS FOR COX PH REGRESSION ANALYSIS    ###################
#########################################################################

# Add pb_20333.2008 per Pagano et al. 2012
# Add pb_20525.2014 per ice_arrive_depart.csv
# I called both bears "undecided" but am now including

library(sf)
library(raster)
library(dplyr)
library(landscapemetrics)
library(lsm)
library(tmap)

load("all_v2.RData")
load("logreg.RData")

# ----  Add departure dates to all.v2 ----------- #

#all.v2$ymd <- as.character(all.v2$ymd)
#all.v2$id.ymd <- paste(all.v2$id, all.v2$ymd)

#which(all.v2$id.ymd == "pb_20333.2008 2008-08-15") # scrolled through this date to find entry that matches Pagano et al. 2012
#which(all.v2$id.datetime == "pb_20525.2014 2014-08-14 16:00:29")
#all.v2[11495,]$start.swim <- 1
#all.v2[53228,]$start.swim <- 1

#save(all.v2, file = "all_v2.RData")

# ---  Add ice information  ------------------------------------------------ #

# Prep Data 

# Projections

polar.stereo <- CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs') # EPSG 3413 

rasterlist <- list.files('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/MASIE/2008', full.names = TRUE) # bring in all GeoTIFFs by bear


lb <- subset(all.v2, land_bear == 1)

ss <- subset(lb, start.swim == 1) # 18 swims after adding data below
ss <- unique(ss$id) 

bears <- subset(lb, lb$id %in% ss)
bears <- filter(bears, month > 5 & month < 10) 
bears$ordinal <- yday(bears$ymd)
bears$ord.year <- paste(bears$year, bears$ordinal, sep = "")

new <- subset(bears, id == "pb_20333.2008" 
              #| id == "pb_20525.2014"
)

#---------------- CREATE SPATIAL DATA ---------------------#

# create spdf using sp

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(new$X, new$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = new, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

tmap_mode("plot")

tm_shape(pb.spdf.polar) +  # looks good!
  tm_symbols()


# separate date component of TIF name to correspond to spdf metadata 

stack <- list()
date<-vector()
for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  tt<-unlist(strsplit(names(stack[[i]]), "[_]"))
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
}

st <- stack(stack)

# --------------------------------------------------------------------------------------------------------- #

check_landscape(st[[1]]) # rasters look good
#list_lsm(level = "class") # to see all class level metrics

# Try using lsm 

sample_lsm(st[[1]], pb.spdf.polar[1:2,], plot_id = new$id.datetime[1:2], shape = "circle", size = 30000, verbose = TRUE, what = c("lsm_c_area_mn", "lsm_c_pland")) # works better!

cs <- list()
for (i in 1:nrow(pb.spdf.polar)) {
  st2<-st[[which(date==pb.spdf.polar$ord.year[i])]]
  cs[[i]] <- sample_lsm(st2, pb.spdf.polar[i,], plot_id = pb.spdf.polar$id.datetime[i], shape = "circle", size = 30000, verbose = TRUE, 
                        what = c("lsm_c_area_mn", 
                                 "lsm_c_pland",
                                 "lsm_c_te"))
}

cs.df <- do.call(rbind.data.frame, cs)
