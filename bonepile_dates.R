###############################################################################
####    ADDING BONEPILE DATA TO DATABASE      #################################
###############################################################################


# http://www.north-slope.org/departments/wildlife-management/studies-and-research-projects/bowhead-whales/bowhead-whale-subsistence-harvest-research#pubs
# Scroll to end of reports - table says what dates whales were landed

# 0 = no whale landed
# 1 = whale landed

# Assumptions:
  # whales were butchered on same day as landed (check this)
  # bonepiles were lumped together: doesn't matter if was Barrow, Cross Island, or Kaktovik

rm(list = ls())

library(lubridate)
library(dplyr)

#-------------- FORMAT DATA -------------------------------------------------------------------------- #

bone <- read.csv("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Data/Bonepile_Dates.csv")

bone$Dates <- mdy(bone$Dates)
bonedates <- as.character(bone$Dates)

load("land_bears_CoxPH.RData")

bears <- bears %>%
  mutate(Bonepile = if_else(ymd %in% bonedates, 1, 0))

test <- subset(bears, Bonepile == 1) # looks good

save(bears, file = "land_bears_CoxPH.RData")





