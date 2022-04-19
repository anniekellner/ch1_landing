########################################################
##    GPS COLLAR DATA  ################
########################################################

library(dplyr)


rm(list = ls())

# Load original csv - import from raw-data

csv <- usgs_pbear_gps_ccde16_v20170131
rm(usgs_pbear_gps_ccde16_v20170131)

csv$id <- paste(csv$animal, csv$year, sep = '.')

## How many collars were deployed?

unique(csv$id)

# How many ice bears?

load('./data/RData/all_v2.RData')

land <- dplyr::filter(all.v2, land_bear_ows == 1)

unique(land$id)
