#########################################################################
####      ADD BEARS FOR COX PH REGRESSION ANALYSIS    ###################
#########################################################################

# Section 1: add departure dates to all.v2 database
# Section 2: add patch metrics
# Section 3: add SIC_30m_me, SIC_30m_max, SIC_30m_min

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

# MASIE - for patch metrics

rasterlist <- list.files('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/MASIE/2014', full.names = TRUE) # bring in all GeoTIFFs by bear


lb <- subset(all.v2, land_bear == 1)

ss <- subset(lb, start.swim == 1) # 18 swims after adding data below
ss <- unique(ss$id) 

bears <- subset(lb, lb$id %in% ss)
bears <- filter(bears, month > 5 & month < 10) 
bears$ordinal <- yday(bears$ymd)
bears$ord.year <- paste(bears$year, bears$ordinal, sep = "")

new <- subset(bears, id == "pb_20525.2014")

new <- new %>% 
  filter(!(ord.year=='2014184'))

#---------------- CREATE SPATIAL DATA ---------------------#

# create spdf using sp

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(new$X, new$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = new, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

tmap_mode("view")

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

sample_lsm(st[[1]], pb.spdf.polar[1402,], plot_id = new$id.datetime[1402], shape = "circle", size = 30000, verbose = TRUE, what = c("lsm_c_area_mn", "lsm_c_pland")) # works better!

cs <- list()
for (i in 1:nrow(pb.spdf.polar)) {
  st2<-st[[which(date==pb.spdf.polar$ord.year[i])]]
  cs[[i]] <- sample_lsm(st2, pb.spdf.polar[i,], plot_id = pb.spdf.polar$id.datetime[i], shape = "circle", size = 30000, verbose = TRUE, 
                        what = c("lsm_c_area_mn", "lsm_c_pland",
                                 "lsm_c_te"))
}

cs.df2 <- do.call(rbind.data.frame, cs)

save(cs.df, file = "added_bears_CoxPH.RData")

# Add 

load("added_bears_CoxPH.RData")

tail(cs.df) # see whether all dates were calculated - looks good

newBears <- rbind(cs.df, cs.df2)

save(newBears, file = "added_bears_CoxPH.RData")



##########################################################################################################################################

rm(list = ls())

library(sf)
library(raster)
library(dplyr)
library(tmap)
library(lubridate)

#load('SIC_KFM.RData')
load("all_v2.RData")
#load("logreg.RData")

lb <- subset(all.v2, land_bear == 1)

ss <- subset(lb, start.swim == 1) # 18 swims after adding data below
ss <- unique(ss$id)

bears <- subset(lb, lb$id %in% ss)
bears <- filter(bears, month > 5 & month < 10) 

new <- subset(bears, id == "pb_20525.2014")


#---------------- SPATIAL DATA ---------------------#

# AMSRE-2

rasterlist <- list.files('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/OWS_2014', pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=TRUE)

test <- raster(rasterlist[100])
plot(test)

# create spdf using sp

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +x_0=0 +y_0=0 +a=6378273 +rf=298.279411123064 +units=m +no_defs') # matches AMSRE raster
coords <- cbind(new$X, new$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = new, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic

pb.spdf.polar$ymd <- ymd(pb.spdf.polar$ymd)
pb.spdf.polar$date2 <- format(pb.spdf.polar$ymd, "%Y%m%d")


# Plot
test <- raster(rasterlist[[100]])

plot(test)
plot(pb.spdf.polar)

tm_shape(pb.spdf.polar) +
  tm_symbols()


# separate date component of TIF name to correspond to spdf metadata 
# Data@names has a "." instead of a "_" in the names slot

stack<-list()
date<-vector()

for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  stack[[i]][stack[[i]] == 120] <- NA
  tt<-unlist(strsplit(names(stack[[i]]), '[[:punct:]]+')) #https://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
}

st <- stack(stack)


st[st == 120] <- NA




# for loop that runs through each point and pulls data from appropriate GeoTIFF

pb.spdf.polar$SIC_30m_me <- NULL
pb.spdf.polar$SIC_30m_max <- NULL
pb.spdf.polar$SIC_30m_min <- NULL

for (i in 1:nrow(pb.spdf.polar)) {
  st2<-st[[which(date==pb.spdf.polar$date2[i])]]
  pb.spdf.polar$SIC_30m_me[i]<-extract(st2, pb.spdf.polar[i,], buffer = 30000, fun = mean, na.rm = TRUE)
  pb.spdf.polar$SIC_30m_max[i]<-extract(st2, pb.spdf.polar[i,], buffer = 30000, fun = max, na.rm = TRUE)
  pb.spdf.polar$SIC_30m_min[i]<-extract(st2, pb.spdf.polar[i,], buffer = 30000, fun = min, na.rm = TRUE)
}

new <- pb.spdf.polar@data

tm_shape(test) + 
   tm_raster()


# --- NOT USED  ---------------------------------------------------------------------------------------------------------------- #

head(newBears)

area_mn_10 <- area_mn_10 %>% pivot_wider(names_from = c(class), values_from = c(class, value)) # spread rows into columns by class


full <- left_join(ice.df, area_mn_10, by = "id.datetime")# join ice.df with lsm df's

# Change ice value (3) to 0 if NA
full$value_3[is.na(full$value_3)] <- 0
full$class_3[is.na(full$class_3)] <- 3

full$leave.ice <- ifelse(full$index==0,1,0) # day they depart = 1
