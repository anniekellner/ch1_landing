rm(list = ls())

library(raster)

x <- raster('./SIC-TIFs/MASIE/pb_06817/masie_ice_r00_v01_2006236_4km.tif')

x[3000,3000] # value=3
x[100,100]


#test this code to see whether 3 <- 1
df <- data.frame(id=3, v=1)
x2 <- subs(x, df, subsWithNA=FALSE)

x2[3000,3000] # 1
x2[100,100] # NA



