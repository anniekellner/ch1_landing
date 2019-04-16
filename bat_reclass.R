###############################################################
###         BATCH RECLASSIFY AND CLUMP RASTERS        ##########
###############################################################

## Calculate distance from bears location to nearest pack ice ##

# ----------------- Load Data --------------------------------------#

rm(list = ls())

library(raster)

rl <- list.files(path = "./SIC-TIFs/SIC_univ_Bremen/pb_06817", pattern='.tif', all.files=TRUE, full.names=FALSE)

m <- c(1,15,0, 15,100,1, 100,255,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

batch_reclass <- function(rl){
  for (i in 1:length(rl)) {
    r <- raster(paste0("./SIC-TIFs/SIC_univ_Bremen/pb_06817/", rl[i])) #read in raster
    rc <- reclassify(r, rclmat) #reclassify such that SIC>15% = 1, else 0
    rcc <- clump(rc, directions=8) # clumping algorithm
    writeRaster(rcc, filename = paste0("./SIC-TIFs/SIC_univ_Bremen/RCC/", "rcc_", 
                                       rl[i]), format="GTiff", overwrite=TRUE)
  }
}

#run the function
batch_reclass(rl)

### TEST

test <- raster('./SIC-TIFs/SIC_univ_Bremen/RCC/rcc_asi-n6250-20060921-v5.4.tif')
test
plot(test)
#writeRaster(test, './Tests/bat_clump.tif')
