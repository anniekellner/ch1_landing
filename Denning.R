##########################################
##      Denning Bears   ##################
##########################################

# See whether data exists for bears during year when they enter den, have a coy, have a yearling
# Add reproductive status to all.v2 database
# Account for loss of coy (revise status)


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

# ---------------------------------------------------------------------------------------------------------------------------- #

den$id.coy <- paste(den$BearID, den$Den_Emerge, sep = '.')
length(which(den$id.coy %in% all.v2$id)) # which bears have data from year they EMERGED from den (coy)

all.v2$coy <- ifelse(all.v2$id %in% den$id.coy, 1, 0) # Add coy to main dataframe


# which bears have data fall before (DenYr)

den$Den_Emerge <- as.numeric(den$Den_Emerge)
den <- den %>% mutate(DenYr = Den_Emerge - 1)
den$id.den <- paste(den$BearID, den$DenYr, sep = '.') 

length(which(den$id.den %in% all.v2$id)) # how many bears have data from year they entered den 

all.v2$DenYr <- ifelse(all.v2$id %in% den$id.den, 1, 0) # add DenYr to main df

# which bears have yearlings

den$Den_Emerge <- as.numeric(den$Den_Emerge)
den <- den %>% mutate(yrAfter = Den_Emerge + 1)
den$id.yearling <- paste(den$BearID, den$yrAfter, sep = '.')

which(den$id.yearling %in% all.v2$id) # which bears have data from year AFTER denning 

all.v2$yearling <- ifelse(all.v2$id %in% den$id.yearling, 1, 0) # add yearling to main df

# Look at data

DenYr <- subset(all.v2, DenYr ==1)
coy <- subset(all.v2, coy==1)
yearling <- subset(all.v2, yearling==1)

# bears that den in consecutive years (account for loss of coy)

all.v2$coy[all.v2$coy == 1 & all.v2$DenYr ==1] <- 0 # change coy to 0 if both coy & denYr

all.v2$yearling[all.v2$animal == 'pb_20446'] <- 0 # change yearling to 0 for pb_20446 (better would have been DenYr ==1 & Yearling == 1)

all.v2$ows <- ifelse(all.v2$month > 5 & all.v2$month < 11,1,0) # create column for ows

#save(all.v2, file = 'all_v2.RData')











