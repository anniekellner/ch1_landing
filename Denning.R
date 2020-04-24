##########################################
##      Denning Bears   ##################
##########################################

# See whether data exists for bears the year they emerged, or the year prior to emergence from den


rm(list = ls())
   
library(stringr)
library(dplyr)
   
load('all_v2.RData')
load('ded_ids.RData')

# import csv - denning data from TA
den <- read.csv('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Data/Denning collared bears_2004-15.csv')
colnames(den) <- c("Den_Emerge", "Subpop", "BearID")
str(den)
den$BearID <- str_trim(den$BearID, side = "right") # trim whitespace

den$id <- paste(den$BearID, den$Den_Emerge, sep = '.')

which(den$id %in% all.v2$id) # which bears have data from year they EMERGED from den
# 2  8 13 14 22 24 25 26 27 28 29 30 31 32 33 34 37


# Fall before
den$Den_Emerge <- as.numeric(den$Den_Emerge)

den <- den %>% mutate(yrb4 = Den_Emerge - 1)

den$id2 <- paste(den$BearID, den$yrb4, sep = '.') 

which(den$id2 %in% all.v2$id) # which bears have data from year BEFORE denning 
#8 10 16 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37

allden <- subset(all.v2, all.v2$id %in% den$id)
allden <- droplevels(allden)
coy <- subset(allden, month > 5 & month < 11)
coy <- droplevels(ows)

unique(coy$id)

preg <- subset(all.v2, all.v2$id %in% den$id2)
preg <- subset(preg, month > 5 & month < 11)
preg <- droplevels(preg)







