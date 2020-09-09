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

ows <- subset(all.v2, ows == 1)

# import csv - denning data from TA
den <- read.csv('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Data/Denning collared bears_2004-15.csv')
colnames(den) <- c("Den_Emerge", "Subpop", "BearID")
str(den)
den$BearID <- str_trim(den$BearID, side = "right") # trim whitespace

# ---------------------------------------------------------------------------------------------------------------------------- #

den$id.coy <- paste(den$BearID, den$Den_Emerge, sep = '.')

length(which(den$id.coy %in% ows$id)) # which bears have data from year they EMERGED from den (coy) # n = 10 in OWS

ows$coy <- ifelse(ows$id %in% den$id.coy, 1, 0) # Add coy to main dataframe

test.coy <- subset(ows, coy == 1)
unique(test.coy$id) # n = 10

# which bears have data fall before (DenYr)

den$Den_Emerge <- as.numeric(den$Den_Emerge)
den <- den %>% mutate(DenYr = Den_Emerge - 1)
den$id.den <- paste(den$BearID, den$DenYr, sep = '.') 

length(which(den$id.den %in% ows$id)) # how many bears have data from year they entered den # 21

ows$DenYr <- ifelse(ows$id %in% den$id.den, 1, 0) # add DenYr to main df

# which bears have yearlings

den$Den_Emerge <- as.numeric(den$Den_Emerge)
den <- den %>% mutate(yrAfter = Den_Emerge + 1)
den$id.yearling <- paste(den$BearID, den$yrAfter, sep = '.')

length(which(den$id.yearling %in% ows$id)) # which bears have data from year AFTER denning 

ows$yearling <- ifelse(ows$id %in% den$id.yearling, 1, 0) # add yearling to main df

# Look at data

DenYr <- subset(ows, DenYr ==1)
unique(DenYr$id)
coy <- subset(ows, coy==1)
unique(coy$id)
yearling <- subset(ows, yearling==1)
unique(yearling$id)

# Bears that lost coys

lost <- subset(ows, DenYr == 1 & coy == 1)
unique(lost$id)


# bears that den in consecutive years (account for loss of coy)
ows$coy <- ifelse(ows$coy == 1 & ows$DenYr == 1, 0, ows$coy) # checks out

ows$yearling <- ifelse(ows$DenYr == 1 & ows$yearling == 1,0, ows$yearling) # checks out

save(all.v2, file = 'all_v2.RData')












