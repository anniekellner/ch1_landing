##################################################################
#####     Defining Departure Dates for Ice Bears  #################
##################################################################

rm(list = ls())

library(dplyr)
library(tmap)

load('all_v2.RData')
load('logreg.RData')

# ----  Data Prep  ------------------ #

icebears <- subset(all.v2, land_bear == 0)
icebears67 <- subset(icebears, month == 6 | month == 7)



# ----- Visualize ------ #



