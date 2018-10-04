rm(list = ls())

load('all.RData')

## Set first distance value for starting point as 0

swim <- subset(all, all$swim==1)
swim$index <- seq(1,nrow(swim),1) # so there is a unique identifier for each row
id <- unique(swim$id)

for (i in 1:length(id)){ # observation i in vector 1:length(id)
  subset <- swim[swim$id==id[i],] # subsets each individual (i) then proceeds through loop 
  row <- subset$index[1] #identifies the first entry 
  swim[row,10] <- 0 #replaces distance value with 0
}

######

swim$distance <- as.numeric(swim$distance)
swim$cumdist <- 0

sums <- data.frame()
ids <- unique(swim$id)
swimLengths = cbind(ids,rep(0,length(ids)))
for (i in ids){
  sub <- swim[swim$id==i,]
  tempSums <- NULL
  for (j in 1:nrow(sub)){
    tempSums[j]=sum(sub$distance[1:j])
  }
  swim$cumdist[swim$id==i]=tempSums
  swimLengths[swimLengths[,1]==i,2]=tempSums[nrow(sub)]
}


