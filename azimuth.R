rm(list = ls())

library(dplyr)

load('all.RData')

# subset by swims, using data from month(s) in which hypothesized swims occur
pb1 <- subset(all, animal=='pb_06817' & month==9 & year==2006)
pb2 <- subset(all, animal=='pb_20333' & month==8 & year==2008)
pb3 <- subset(all, animal=='pb_20413' & month==9 & year==2006)
pb4 <- subset(all, animal=='pb_20414' & month==7 & year==2009)
pb5 <- subset(all, animal=='pb_20418' & month==7 & year==2005)
pb6 <- subset(all, animal=='pb_20418' & month==8 & year==2005)
pb7 <- subset(all, animal=='pb_20446' & month==7 & year==2009)
pb8 <- subset(all, animal=='pb_20520' & month==8 & year==2012)
pb9 <- subset(all, animal=='pb_20525' & month==8 & year==2013)
pb10 <- subset(all, animal=='pb_20525' & month==8 & year==2014)
pb11 <- subset(all, animal=='pb_20529' & month==8 & year==2004)
pb12 <- subset(all, animal=='pb_20529' & month==9 & year==2005)
pb13 <- subset(all, animal=='pb_20735' & month==7 & year==2009)
pb14 <- subset(all, animal=='pb_20845' & month==8 & year==2015)
pb15 <- subset(all, animal=='pb_21015' & month==8 & year==2013)
pb16 <- subset(all, animal=='pb_21264' & month==7 & year==2011)
pb17 <- subset(all, animal=='pb_21264' & month==8 & year==2011)
pb18 <- subset(all, animal=='pb_21358' & month==8 & year==2013)
pb19 <- subset(all, animal=='pb_21368' & month==8 & year==2014)
pb20 <- subset(all, animal=='pb_32366' & month==8 & year==2011)
pb21 <- subset(all, animal=='pb_32366' & month==8 & year==2014)
pb22 <- subset(all, animal=='pb_20520' & month==7 & year==2012)

swim.az <- rbind(pb1, pb2, pb3, pb4, pb5, pb6, pb7, pb8, pb9, pb10, pb11, pb12, pb13, pb14, pb15, pb16, pb17, pb18, pb19, pb20, pb21, pb22)
swim.az$ID <- paste(swim.az$animal, swim.az$year, sep = '.')

# Set first azimuth at 0
swim.az$index <- seq(1,nrow(swim.az),1) # so there is a unique identifier for each row

id <- unique(swim.az$ID)

for (i in 1:length(id)){ # observation i in vector 1:length(id)
  subset <- swim.az[swim.az$ID==id[i],] # [swim.az$ID==id[i]] -> subsets each individual (i) then proceeds through loop (total=20 df's)
  row <- subset$index[1] #identifies the first entry in subset$index (top row within each subsetted group i)
  swim.az[row,12] <- 'NA' #replaces value with NA
}
 
# classify as southward or not  
# classification: ESE (101.25) - WSW (258.75)  

swim.az$south <- ifelse(swim.az$azimuth > 101.25 & swim.az$azimuth < 258.75, 1, 0)

#eliminate unnecessary columns
library(dplyr)
swim.az <- select(swim.az, c(animal:gps_lon, distance, azimuth, datetime, ID, land, south))

# add cumdist

library(data.table)
setDT(swim.az)

swim.az[, cumdist := south*cumsum(distance), .(animal, rleid(south))]

swim.az$cumdist <- format(round(swim.az$cumdist, 3), nsmall = 3)



save(swim.az, file='swim.az.RData') #save as .RData
