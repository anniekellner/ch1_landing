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

load("land_bears_CoxPH.RData")

bears <- bears %>%
  select(id, animal:gps_lon, datetime, land, start.swim, X, Y, ymd, go_into_den:yearling)

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

bears$repro <- 0

bears <- bears %>%
  mutate(repro = replace(repro, go_into_den == 1, 1)) %>%
  mutate(repro = replace(repro, coy == 1, 2)) %>%
  mutate(repro = replace(repro, yearling == 1, 3))

bears$repro <- factor(bears$repro, labels = c("Unknown", "Enter_Den", "COY", "Yearling"))

bears <- left_join(bears, biolog)

# ---------------------------------------------------------------------------------------- #

# Coxph analysis with biological variables

source('MyFunctions.R') # AICC

bears <- bears %>% 
  group_by(id, ymd) %>%
  mutate(ordinal = yday(ymd)) %>%
  dplyr::summarise(
    first(animal), first(ordinal), max(start.swim), first(repro), first(age), first(ResidualMass), max(start.swim)
  )

colnames(bears) <- c("id", "ymd", "animal", "ordinal", "start.swim", "repro", "age", "resid_mass")

# --  RUN MODELS ----------------------------------------------------------------------------------------------- #

repro <- coxph(Surv(ordinal, start.swim) ~ repro, data = bears)
age <- coxph(Surv(ordinal, start.swim) ~ age, data = bears)
rm <- coxph(Surv(ordinal, start.swim) ~ resid_mass, data = bears)
repro_age <- coxph(Surv(ordinal, start.swim) ~ repro + age, data = bears)
repro_rm <- coxph(Surv(ordinal, start.swim) ~ repro + resid_mass, data = bears)
age_rm <- coxph(Surv(ordinal, start.swim) ~ age + resid_mass, data = bears)
repro_age_rm <- coxph(Surv(ordinal, start.swim) ~ repro + age + resid_mass, data = bears)

aicc <- AICc(repro, age, rm, repro_age, repro_rm, age_rm, repro_age_rm)

create_AICc_table(aicc)

summary(age)

# CONCLUSION: No biological variables affect the TIMING of migration 

# ------------------------------------------------------------------------------------------------------- #
 # COX REGRESSION WITH BIOLOGICAL VARIABLES #

