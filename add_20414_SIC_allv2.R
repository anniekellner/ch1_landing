#########################################################################
### merge SIC with main database after missing one sample (pb_20414) ####
#########################################################################

load('all_v2.RData')

library(dplyr)

SIC <- subset(all.v2, SIC>=0) #subset SIC records

unique(SIC$id) # missing pb_20414

## Run pb_20414 through SIC.R 

SIC <- rbind(SIC, df) #bind pb_20414 to SIC

all.v2 <- select(all.v2, -SIC)

all.v2 <- left_join(all.v2, SIC)

save(all.v2, file='all_v2.RData')
