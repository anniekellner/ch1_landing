##########################################
##      Denning Bears   ##################
##########################################

# See whether data exists for bears during year when they enter den, have a coy, have a yearling
# Add reproductive status to all.v2 database
# Account for loss of coy (revise status)


rm(list = ls())
   
library(stringr)
library(dplyr)
library(lubridate)
   
#load('all_v2.RData')
load('land_bears_CoxPH.RData')

# Import csv - denning data from TA (Lillie et al. 2018)

den <- read.csv('C:/Users/akell/Documents/PhD/Polar_Bears/Data/Repro/Repro.csv')
den <- select(den, 1,2,4,5,6)
den$id <- paste(den$animal, den$year, sep = '.')

# Remove denning data from Cox df

bears <- select(bears, -c("coy", "DenYr", "yearling"))

# create df's for repro status

go_into_den <- subset(den, DenYr == 1)

coy <- subset(den, coy == 1)

yearling <- subset(den, yearling == 1)


# Add columns to Cox df

bears  <- bears %>%
  mutate(go_into_den = ifelse(id %in% go_into_den$id, 1, 0)) %>%
  mutate(coy = ifelse(id %in% coy$id, 1, 0)) %>%
  mutate(yearling = ifelse(id %in% yearling$id, 1, 0))

save(bears, file = 'land_bears_CoxPH.RData')



















