
#load and filter data
load('all.RData')
swims <- subset(all, start.swim==1 | end.swim==1 | swim==1)

##### create traj in adehabitat ######
#####################################

library(adehabitatLT)

# convert LL to XY
library(proj4)

M <- as.matrix(cbind(swims$gps_lon, swims$gps_lat)) #create a matrix to project
xy <- project(M, "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #project to NAD 1983 Alaska Albers
X <- xy[,1]
Y <- xy[,2]

#bind the datasets
swims2 <- cbind(swims,X,Y)

swim.traj <- as.ltraj(xy=swims2[c("X","Y")], date = swims2$datetime, id=swims2$id) # create traj file

# create spatial line dataframe
library(sp)
library(rgdal)
sldf <- ltraj2sldf(swim.traj, byid=FALSE)


writeOGR(sldf, dsn = "./Shapefiles", layer = "migration", driver = "ESRI Shapefile")




