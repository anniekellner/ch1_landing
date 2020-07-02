###############################################################
###         BATCH RECLASSIFY AND CLUMP RASTERS        ##########
###############################################################

## Reclassify SIC rasters so 1=ice and 0=else. Use clump fxn to determine pack ice ##

# ----------------- Load Data --------------------------------------#

rm(list = ls())

library(raster)

#------------------ Create directories for new folders -------------#

#load('ded_ids.RData')

#for(i in 1:length(ded)){
#dir.create(paste0('./SIC-TIFs/SIC_univ_Bremen/RCC/', ded[i]))
#}

setwd("C:/Users/akell/Documents/PhD/Polar_Bears/Data/SIC-TIFs/SIC_univ_Bremen/n3125")

rl <- dir(path = "./All", pattern='.tif', all.files=TRUE, recursive = TRUE, full.names=FALSE)

m <- c(1,15,0, 15,100,1, 100,255,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

batch_reclass <- function(rl){
  for (i in 1:length(rl)) {
    r <- raster(paste0("./All/", rl[i])) #read in raster
    rc <- reclassify(r, rclmat) #reclassify such that SIC>15% = 1, else 0
    rcc <- clump(rc, directions=8) # clumping algorithm
    writeRaster(rcc, filename = paste0("./All/RCC/", 
                                       rl[i]), format="GTiff", overwrite=TRUE)
  }
}

#run the function
batch_reclass(rl)

# ------------------------- TEST ------------------------------------------------#

test <- raster('./SIC-TIFs/RCC/pb_20735.2009/asi-n6250-20090725-v5.4.tif')
test
plot(test)
#writeRaster(test, './Tests/bat_clump.tif')
