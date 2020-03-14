###################################################################################
##          CALCULATE GREAT CIRCLE DISTANCE BETWEEN START AND END OF MIGRATION    #
###################################################################################

# Compared against ArcGIS: looks good!!!


rm(list = ls())

library(geosphere)
library(dplyr)
load('all.RData')

## Set first distance value for starting point as 0

swim <- subset(all.v2, all.v2$swim==1)
swim$index <- seq(1,nrow(swim),1) # so there is a unique identifier for each row
id <- unique(swim$id)

for (i in 1:length(id)){ # observation i in vector 1:length(id)
  subset <- swim[swim$id==id[i],] # subsets each individual (i) then proceeds through loop 
  row <- subset$index[1] #identifies the first entry 
  swim[row,10] <- 0 #replaces distance value with 0
}

## Extract starting and ending LL
out <- data.frame()
for (i in 1:length(id)){
  sub <- swim[swim$id==id[i],]
  start<-sub[1,]
  end<-sub[nrow(sub),]
  start$lat_end<-end$gps_lat
  start$lon_end<-end$gps_lon
  out <- rbind(out, start)
}

start.end.LL <- dplyr::select(out, id, gps_lon, gps_lat, lon_end, lat_end) #select relevant columns for df

## Calculate straight-line distance

get.dist <- function(i){
  row <- start.end.LL[i,]
  dist <- distGeo(c(start.end.LL$gps_lon[i], start.end.LL$gps_lat[i]), c(start.end.LL$lon_end[i], start.end.LL$lat_end[i]))
  return(dist)
  } 
start.end.LL$Distance <- lapply(X=seq(1,nrow(start.end.LL),1), FUN = get.dist)

save(start.end.LL, file='Str_line_dist.RData')

#distm(c(-154.4895, 71.5196), c(-154.4969, 71.5019)) #test to see whether distm() fxn is equivalent to TA distance.It is.





