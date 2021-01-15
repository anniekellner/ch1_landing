######################################################
###   ADD BIOLOGICAL VARIABLES TO COX DATABASE  ######
######################################################

library(dplyr)
library(lubridate)
library(survival)
library(MuMIn)


rm(list = ls())

# Prep data
# Land bear df

bears <- readRDS("./data/RData/land_bears_CoxPH.Rds")

bears2 <- bears %>%
  select(id.x, animal, year, datetime.x, start.swim, go_into_den: yearling, pland, te, dist_to_ice, ymd, go_into_den:yearling)

colnames(bears2) <- c("id", "animal", "year", "datetime", "start.swim", "den", "coy", "yearling", "pland", "te", "dist_to_ice", "ymd")

# Biological vars from .csv (T. Atwood)

bio1 <- read.csv('./data/raw-data/resid_mass_indfem_19832015.csv')
bio2 <- read.csv('./data/raw-data/Residual_body_masses_additional_bears_20210102.csv')

bio1 <- select(bio1, BearID, capture.year, age, ResidualMass)

bio1$animal <- paste0("pb_", bio1$BearID)
bio1$id <- paste(bio1$animal, bio1$capture.year, sep = ".")

bio1 <- bio1 %>%
  select(-c(BearID, capture.year))

bio2$id <- paste(bio2$ï..animal, bio2$CapYr, sep = '.')

bio2 <- bio2 %>%
  select(-c(Comment, CapYr)) 

colnames(bio2) <- c("animal", "ResidualMass", "age", "id")

biolog <- rbind(bio1, bio2)

# ------------------------------------------------------------------------------------ #

# Repro data 

bears2$repro <- 0

bears2 <- bears2 %>%
  mutate(repro = replace(repro, den == 1, 1)) %>%
  mutate(repro = replace(repro, coy == 1, 2)) %>%
  mutate(repro = replace(repro, yearling == 1, 3))

bears2$repro <- factor(bears2$repro, labels = c("Unknown", "Enter_Den", "COY", "Yearling"))

bears2 <- left_join(bears2, biolog)

bears <- left_join(bears, bears2)




# CONCLUSION: No biological variables affect the TIMING of migration 

# ------------------------------------------------------------------------------------------------------- #
 # COX REGRESSION WITH BIOLOGICAL VARIABLES #

saveRDS(bears, file = './data/RData/land_bears_CoxPH.Rds')
