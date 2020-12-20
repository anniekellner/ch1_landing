##############################################
##    TOTAL EDGE    ##########################
##############################################

library(raster)
library(sf)
library(lubridate)
library(landscapemetrics)
library(tidyr)
library(dplyr)

rm(list = ls())

load('land_bears_CoxPH.RData')
load('lsm.RData')

size = 30000 # buffer size in meters

bears <- distinct(bears)

rasterlist <- list.files('D:/Polar Bears/Data/SIC-TIFs/MASIE', full.names = TRUE) 

# Spatial data

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(bears$X, bears$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = bears, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic


track <- st_as_sf(pb.spdf.polar) # as sf object 

# Add column for year + ordinal date to match MASIE file name

track$ymd <- ymd(track$ymd)
track$ordinal <- yday(track$ymd) #change ymd to ordinal date
track$ord.year <- paste(track$year, track$ordinal, sep="")

# Make sure data looks good

test_ras <- raster(rasterlist[1])
test_bear <- track[1,]

plot(test_ras)
plot(test_bear, add = TRUE)

# Make sure calculation makes sense

pland <- data.frame()
pland <- sample_lsm(test_ras, test_bear, shape = "circle", size = 30000, verbose = TRUE, what = "lsm_c_te") # works better!


# Recalculate TE and PLAND for all bears in Cox reg June 1 - Sept 30

patch_stats <- data.frame()

for (i in 1:nrow(track)) {
  file <- rasterlist[grep(track$ord.year[i], rasterlist)]
  raster <- raster(file)
  result <- sample_lsm(raster, track[i,], plot_id = track$id.datetime[i], shape = "circle", size = size, verbose = TRUE, 
                      what = c("lsm_c_pland", "lsm_c_te"))
  patch_stats <- rbind(patch_stats, result)
}

# Format lsm data

ps <- separate(patch_stats, col = "plot_id", into = c("id", "date", "time"), sep = " ", remove = FALSE)
ps$datetime <- paste(ps$date, ps$time); ps$datetime <- as.POSIXct(ps$datetime) # add new column for datetime
ps$date <- ymd(ps$date)

ps <- ps %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(index = difftime(last(date), date, units = "days"))

ps <- ps %>%
  complete(index, class, fill = list(value = 0)) %>%
  filter(class == 3)

ps_piv <- ps %>% pivot_wider(names_from = c(class), values_from = c(class, value)) # spread rows into columns by class
ps_piv <- rename(ps_piv, id.datetime = plot_id)

full <- left_join(bears, ps_piv, by = "id.datetime") # join ice.df with lsm df's

# Change ice value (3) to 0 if NA

full$value_3[is.na(full$value_3)] <- 0
full$class_3[is.na(full$class_3)] <- 3

which(is.na(full$value_3)) # Not sure why so many NA's...need to figure out

