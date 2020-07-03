######################################################################################
##########      DISTANCE DATA FOR KFM     ############################################
######################################################################################

# Compile Distance-to-shore and Distance-to-ice pack data for all bears in analysis

rm(list = ls())

library(sf)
library(sp)
library(dplyr)
library(tmap)
library(lwgeom)
library(proj4)
library(raster)
library(MuMIn)

# ----  BEAR SPATIAL DATA ---------------------------------------------------------------------------------------------------- #

# Load bear data

load("logreg.RData") # loads in polar stereo projection
logreg <- SIC; rm(SIC)

polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') 

DFtoSF <- function(df, projection) {
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.project <- st_transform(sf.wgs, projection)
  return(sf.project)
}

logreg <- DFtoSF(logreg, polar.stereo)


# ------- LAND SPATIAL DATA  ---------------------------------------------------------------------------------------------- #

setwd('C:/Users/akell/Documents/ArcGIS')

bbox <- st_read('./Land Shapefiles/bbox.shp')

land <- st_read('./Land Shapefiles/AK_CA_5kbuff.shp')
land.dissolve <- land %>% summarise(Shape_Area = sum(Shape_Area)) # dissolve AK and territories into one obj
plot(st_geometry(land.dissolve)) # OK

# see where bbox overlaps shapefile

tm_shape(land.dissolve) +
  tm_polygons() +
  tm_shape(bbox) +
  tm_polygons()

land.crop <- st_crop(land.dissolve, xmin = -596311, ymin = 2083889, xmax = 850962, ymax = 3576142) # crop shapefile
land.ps <- st_transform(land.crop, polar.stereo)

# Verify land and points look good

tm_shape(logreg) + # plot points first so frame is large enough to accompany points and land
  tm_dots() +
  tm_shape(land.ps) +
  tm_fill()

# Check bear points in Chukchi Sea
#THey are early June, bear pb_20797. Leaving in because I doubt it will make a difference and not worth removing

tmap_mode("view")

tm_shape(logreg) +
  tm_dots(popup.vars = c("id", "ymd")) +
  tm_shape(land.ps) +
  tm_fill()

# ---------   DISTANCE ANALYSIS   ------------------------------------------ #

dist <- vector()
dist <- st_distance(logreg, land.ps, by_element = TRUE)

logreg$dist2land <- cbind(matrix(dist))

save(logreg, file = "C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/logreg.RData")


# ------  LOGISTIC REGRESSION  ------------------------------------------------------------------------ #

# Use only one location per day (last GPS location)

loc1 <- logreg %>% # if there were both on and off land observations in a single day, take last
  group_by(id, ymd) %>%
  arrange(id.datetime) %>%
  slice(n()) # select last row
  



fit_d2l <- glm(leave.ice ~ dist2land, data = logreg, family = binomial())
summary(fit_d2l)

fit_max <- glm(leave.ice ~ SIC_30m_max, data = logreg, family = binomial())
summary(fit_max)

fit_repro <- glm(leave.ice ~ repro, data = logreg, family = binomial())
summary(fit_repro)

fit_repro_max <- glm(leave.ice ~ repro + SIC_30m_max, data = logreg, family = binomial())

fit_d2l_max <- glm(leave.ice ~ dist2land + SIC_30m_max, data = logreg, family = binomial())

fit_d2l_max_repro <- glm(leave.ice ~ repro + SIC_30m_max + dist2land, data = logreg, family = binomial())

aicc <- AICc(fit_max, fit_repro, fit_repro_max, fit_d2l_max, fit_d2l_max_repro)

# Create AIC table to compare models
#AICc should be used instead AIC when sample size is small 
#in comparison to the number of estimated parameters 
#(Burnham & Anderson 2002 recommend its use when n/K < 40)

create_AICc_table <- function(aicc){
  aicc$deltaAIC <- aicc$AIC - min(aicc$AIC) 
  aicc$L <- exp(-0.5*(aicc$deltaAIC))
  aicc$weight <- aicc$L/(sum(aicc$L))
  aicc$weight.pct <- aicc$weight*100
  aicc <- arrange(aicc, aicc$deltaAIC)
  return(aicc)
}


