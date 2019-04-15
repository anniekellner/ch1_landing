rm(list = ls())

library(raster)

files = dir('./SIC-TIFs/MASIE', pattern = '.tif', recursive = TRUE, full.names = TRUE)
nr <- vector("list", length(files))
names(nr) <- files


l <- raster(basename(files))

for (i in 1:length(files)) {
  tmp <- raster(files[i])
  df <- data.frame(id=3, v=1)
  nr[[i]] <- subs(tmp, df, subsWithNA=FALSE)}
  

#List all of the 365 sub-directories within my main directory
days <- list.files(full.names = F , recursive =F, pattern='*X2000*')

#Apply my function to each directory within "days"
for(j in 1:length(days)){
  
  named <- paste0("full_",j)
  
  in.list <- list.files(recursive = T, full.names = F)
  
  stitched <- mosaicList(in.list)
  
  writeRaster(stitched, path='D:/Scratch/DataConvert/Daymet_Data/Full/' , 
              filename=named, overwrite=TRUE)
}  

rm(list = ls())

library(raster)

#List all of the sub-directories within my main directory
folders <- list.files('./SIC-TIFs/MASIE', pattern='pb', full.names = F , recursive =F)


#Apply my function to each directory within "days"
for(i in 1:length(folders)){
  files <- list.files(path=folders[i], recursive = T, full.names = T)
  r <- raster()
  
  
  stack[[i]]<-raster(rasterlist[i])
  lst <- raster(files[i])
  df <- data.frame(id=3, v=1)
  nr[[i]] <- subs(tmp, df, subsWithNA=FALSE)}

stitched <- mosaicList(in.list)

writeRaster(stitched, path='D:/Scratch/DataConvert/Daymet_Data/Full/' , 
            filename=named, overwrite=TRUE)
}

