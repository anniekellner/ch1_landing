############################################################################
#######     LANDSCAPE METRICS USING PACKAGE LSM       ######################
############################################################################


# MASIE FTP: sidads.colorado.edu Directory: /pub/DATASETS/NOAA/G02186/geotiff/4km
# username: anonymous
# pw: annie.kellner@colostate.edu

rm(list = ls())

library(sp)
library(sf)
library(raster)
library(dplyr)
library(landscapemetrics)

is2009 = FALSE
size = 10000

load('Ice_Measurements.RData')
load('ded_ids.RData')
load('lsm.Rdata')
pb.df <- subset(ice.df, id=='pb_20525.2013')

if(is2009){
  pb.df <- pb.df %>% 
    filter(!(ord.year=='2009181'))} # date is missing from MASIE data

pb.df <- droplevels(pb.df)

rasterlist <- list.files('C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/MASIE/pb_20525', full.names = TRUE) # bring in all GeoTIFFs by bear

#---------------- CREATE SPATIAL DATA ---------------------#

# create spdf using sp

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar.stereo <-CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-80 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +units=m + datum=WGS84 +no_defs +towgs84=0,0,0') # matches MASIE raster
coords <- cbind(pb.df$X, pb.df$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data = pb.df, proj4string = projection) 
pb.spdf.polar <-spTransform(pb.spdf, polar.stereo) #reproject points to polar stereographic


track <- st_as_sf(pb.spdf.polar) # as sf object 
plot(st_geometry(track)) # verify track looks good
#------------ANALYSIS-----------------------------#

# separate date component of TIF name to correspond to spdf metadata 

stack <- list()
date<-vector()
for (i in 1:length(rasterlist)) {
  stack[[i]]<-raster(rasterlist[i])
  tt<-unlist(strsplit(names(stack[[i]]), "[_]"))
  date[i]<-tt[which(nchar(tt)==max(nchar(tt)))]
}

st <- stack(stack)

#check_landscape(st[[1]]) # rasters look good
#list_lsm(level = "class") # to see all class level metrics

# Try using lsm 
#test <- scale_sample(st[[1]], pb.spdf.polar[1:2,],  size = 10000, max_size = 50000, level = "class") # this works!
#sample_lsm(st[[1]], track[1:2,], plot_id = track$id.datetime[1:2], shape = "circle", size = 10000, verbose = TRUE, what = c("lsm_c_area_mn", "lsm_c_pland")) # works better!



cs <- list()
for (i in 1:nrow(track)) {
  st2<-st[[which(date==track$ord.year[i])]]
  cs[[i]] <- sample_lsm(st2, track[i,], plot_id = track$id.datetime[i], shape = "circle", size = size, verbose = TRUE, 
                        what = c("lsm_c_area_mn", 
                                 "lsm_c_ca", 
                                 "lsm_c_cai_mn", 
                                 "lsm_c_clumpy",
                                 "lsm_c_cohesion",
                                 "lsm_c_core_mn",
                                 "lsm_c_ed",
                                 "lsm_c_frac_mn",
                                 "lsm_c_gyrate_mn",
                                 "lsm_c_lpi",
                                 "lsm_c_np",
                                 "lsm_c_para_mn",
                                 "lsm_c_pland",
                                 "lsm_c_te"))
}

cs.df <- do.call(rbind.data.frame, cs) # convert list to dataframe
cs.df$radius_m <- size # change to size parameter from loop

####  Add missing data 04/01/2020  #######
 

cs.df <- dplyr::select(cs.df, -id) # remove id column so can do anti-join
missing <- anti_join(cs.df, lsm)

lsm <- bind_rows(lsm, missing)

save(lsm, file = "lsm.RData") 


#################################################################################################################################################
   
sample_lsm(st[[1]], track[1:2,], plot_id = track$id.datetime[1:2], shape = "circle", size = 10000, verbose = TRUE, what = c("lsm_c_area_mn", "lsm_c_pland"))
  



