##############################################
##    TOTAL EDGE    ##########################
##############################################

library(raster)
library(sf)
library(lubridate)
library(landscapemetrics)
library(tidyr)
library(dplyr)
library(tmap)
library(rasterVis)
library(ggplot2)


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
plot(st_geometry(test_bear), add = TRUE)


# Make sure calculation makes sense

pland <- data.frame()
pland <- sample_lsm(test_ras, test_bear, shape = "circle", size = 30000, verbose = TRUE, what = "lsm_c_te") # works better!

# Remove dates for which no MASIE data exists

track <- track %>%
  filter(!(ord.year == '2008274')) %>%
  filter(!(ord.year == '2012274')) %>%
  filter(!(ord.year == '2009181')) %>%
  filter(!(ord.year == '2009212')) %>%
  filter(!(ord.year == '2009243')) %>%
  filter(!(ord.year == '2009273')) %>%
  filter(!(ord.year == '2012250')) %>%
  filter(!(ord.year == '2012252')) %>%
  filter(!(ord.year == '2014184')) %>%
  filter(!(ord.year == '2014241')) %>%
  filter(!(year < 2006))


# Recalculate TE and PLAND for all bears in Cox reg June 1 - Sept 30

patch_stats2 <- data.frame()

system.time(
  for (i in 1:nrow(track)) {
  file <- rasterlist[grep(track$ord.year[i], rasterlist)]
  raster <- raster(file)
  result <- sample_lsm(raster, track[i,], plot_id = track$id.datetime[i], shape = "circle", size = size, verbose = TRUE, 
                      what = c("lsm_c_pland", "lsm_c_te"))
  patch_stats2 <- rbind(patch_stats2, result)
}
)
# ----- FORMAT DATA -------------------------------------------------------------------------------------------------------------------------------- #

# Format lsm data

load('./data/RData/patch_stats.RData')

ps2 <- ps %>%
  complete(plot_id, nesting(class, metric), fill = list(value = 0)) %>%
  distinct()

ps2 <- separate(ps2, col = "plot_id", into = c("id", "date", "time"), sep = " ", remove = FALSE)
ps2$datetime <- paste(ps2$date, ps2$time); ps2$datetime <- as.POSIXct(ps2$datetime) # add new column for datetime
ps2$date <- ymd(ps2$date)

ps3 <- ps2 %>%
  group_by(date, class, metric) %>%
  #summarise(mean_val = mean(value)) %>%
  filter(class == 3)

ps_piv <- ps3 %>% # spread rows into columns by class
  pivot_wider(names_from = c(metric), values_from = value) %>%
  rename(id.datetime = plot_id)


full <- left_join(bears, ps_piv, by = "id.datetime") # join ice.df with lsm df's



