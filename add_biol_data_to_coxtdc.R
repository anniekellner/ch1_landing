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

load('./data/RData/land_bears_CoxPH.RData')
bears <- full

bears <- distinct(bears)

bears <- bears %>%
  select(animal:yearling, pland, te) %>%
  rename(datetime = datetime.x) %>%
  rename(id = id.x)

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
biolog <- select(biolog, -animal)

# ------------------------------------------------------------------------------------ #

# Repro data 

bears$repro <- 0

bears <- bears %>%
  mutate(repro = replace(repro, go_into_den == 1, 1)) %>%
  mutate(repro = replace(repro, coy == 1, 2)) %>%
  mutate(repro = replace(repro, yearling == 1, 3))

bears$repro <- factor(bears$repro, labels = c("Unknown", "Enter_Den", "COY", "Yearling"))

bears <- distinct(bears)

bears <- semi_join(bears, biolog)


saveRDS(bears, file = './data/RData/land_bears_CoxPH.Rds')
