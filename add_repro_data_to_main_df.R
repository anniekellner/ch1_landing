#############################################################
###   ADD REPRO STATUS TO MAIN DATABASE  ####################
#############################################################

library(dplyr)
library(tidyr)

rm(list = ls())

# -------------- Load data -------------------------------- #

load("land_bears_CoxPH.RData") # land bears with repro data
load('ice_bears_CoxPH.RData') # ice bears - no repro data

mydata <- readRDS('./data/derived-data/all_v2.Rds') # main database

den <- read.csv('C:/Users/akell/OneDrive - Colostate/PhD/Chapter1/Data/Repro/Repro.csv') # denning data from TA (Lillie et al. 2018)

# Add ice/land designation to den dataframe

land_ids <- unique(bears$id)
ice_ids <- unique(ice$id)

# ------------  Create Repro Dataframe  -------------------- #

den <- select(den, 1:9)
den$id <- paste(den$animal, den$year, sep = '.')

den$repro <- NA

den <- den %>%
  mutate(repro = replace(repro, DenYr == 1, "enter_den")) %>%
  mutate(repro = replace(repro, coy == 1, "coy")) %>%
  mutate(repro = replace(repro, yearling == 1, "yearling"))

# Add repro data to all_v2 dataframe

den <- select(den, id, repro)

mydata <- left_join(mydata, den)

saveRDS(mydata, './data/derived-data/all.Rds')

repro <- den %>% # There are NA's in this dataframe, but see what results look like before dealing with them
  mutate(bear_type = case_when(den$id %in% land_ids ~ "land",
                               den$id %in% ice_ids ~ "ice"))

# Deal with NAs - if categorized as 'land_bear' in repro, call it a land bear)
#land_bears <- filter(repro, land_bear == 1)

repro$bear_type <- ifelse(repro$land_bear == 1, "land", repro$bear_type)

# Remove NA's - bears without repro status or no data in open water season

repro <- drop_na(repro) # removed half of all observations

table(repro$repro, repro$bear_type)
count(repro, bear_type)

# Save repro data

saveRDS(repro, './data/derived-data/repro.Rds')
