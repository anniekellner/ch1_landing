################################################################################################
####    GET SIC VALUES FOR V5.4 ################################################################
################################################################################################

# Change cell value of land from 120 to NA

# 2012: No data June 1 - July 1


rm(list = ls())

library(sf)
library(raster)
library(dplyr)
library(tmap)
library(lubridate)

bears <- readRDS("./data/RData/land_bears_cutoff_after_swim.Rds")


#ss <- subset(lb, start.swim == 1) # 18 swims after adding data below
#ss <- unique(ss$id)

#bears <- subset(lb, lb$id %in% ss)
#bears <- filter(bears, month > 5 & month < 10)

#sub <- subset(bears, year == 2014)

#sub$ymd <- ymd(sub$ymd)

# sub <- sub %>% 
  #filter(ymd > '2012-07-02') # data missing before 07-02-2012


#---------------- SPATIAL DATA ---------------------#

rasterlist <- list.files('D:/Polar Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125/All', pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=TRUE)


# create spdf using sp

#projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
#polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +x_0=0 +y_0=0 +a=6378273 +rf=298.279411123064 +units=m +no_defs') # matches AMSRE raster
#coords <- cbind(bears$X, bears$Y)
#pb.spdf <- SpatialPointsDataFrame(coords = coords, data = bears, proj4string = projection) 
#pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic


#pb.spdf.polar$date2 <- format(pb.spdf.polar$ymd, "%Y%m%d")


# separate date component of TIF name to correspond to spdf metadata 
# Data@names has a "." instead of a "_" in the names slot

bears$SIC_mean <- NULL
bears$SIC_max <- NULL
bears$SIC_min <- NULL

bears <- as_Spatial(bears)

# Make sure bears and rasters are same projection; plot

r <- raster(Rasterlist[1])
bears <- spTransform(bears, crs(r))

plot(r)
plot(bears, add = TRUE)

# add columns for new ice data

bears$SIC <- NULL

# Extract data

for (i in 1:nrow(bears)){
  file <- rasterlist[grep(bears$date2[1], rasterlist)]
  r <- raster(file)
  bears$SIC[1] <- extract(r, bears[1,], buffer = 30000, fun = mean, na.rm = TRUE)
}

for (i in 1:nrow(sf)){
  file <- filelist[grep(sf$date2[i], filelist)]
  shp <- st_read(file)
  sf <- st_transform(sf, st_crs(shp))
  sf$dist_to_ice[i] <- st_distance(sf[i,], shp)
}

for (i in 1:nrow(pb.spdf.polar)) {
  st2<-st[[which(date==pb.spdf.polar$date2[i])]]
  pb.spdf.polar$SIC_30m_me[i]<-extract(st2, pb.spdf.polar[i,], buffer = 30000, fun = mean, na.rm = TRUE)
  pb.spdf.polar$SIC_30m_max[i]<-extract(st2, pb.spdf.polar[i,], buffer = 30000, fun = max, na.rm = TRUE)
  pb.spdf.polar$SIC_30m_min[i]<-extract(st2, pb.spdf.polar[i,], buffer = 30000, fun = min, na.rm = TRUE)
}



stack<-list()
date<-vector()

for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  stack[[i]][stack[[i]] == 120] <- NA # change land from 120 to NA in v5.4
  tt<-unlist(strsplit(names(stack[[i]]), '[[:punct:]]+')) #https://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
}

st <- stack(stack)

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


new <- pb.spdf.polar@data # bears just run
new <- select(new, -date2)

head(new)
tail(new)


# Replace bad values in CoxPH df 

head(sub)
sub <- dplyr::select(sub, animal:id.ymd, dist2land, Bonepile)
new <- dplyr::select(new, SIC_30m_me: SIC_30m_min)
bears2014new <- cbind(sub, new)

bears <- filter(bears, year != 2014)

bears <- rbind(bears, bears2014new)

save(bears, file = "land_bears_CoxPH.RData")

