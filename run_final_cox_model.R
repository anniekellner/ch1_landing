##########################################
##    RUNNING THE FINAL COX MODEL ########
##########################################

library(survival)
library(MuMIn)
library(zoo)

rm(list = ls())

cox <- readRDS('./data/RData/cox_tdc.Rds')

# 3-day moving window

cox <- cox %>%
  mutate(pland3 = rollmean(pland, 3, fill = NA, align = "right")) %>%
  mutate(windspeed3 = rollmean(windspeed, 3, fill = NA, align = "right")) %>%
  mutate(te3 = rollmean(te, 3, fill = NA, align = "right")) %>%
  mutate(dist_land3 = rollmean(dist_land, 3, fill = NA, align = "right")) %>%
  mutate(dist_ice3 = rollmean(dist_pack, 3, fill = NA, align = "right")) 

global.model <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3 + te3 + dist_land3 + dist_ice3 + sd7 + repro + rm + year + pland3*windspeed3, 
                      cluster = animal, data = cox) # no age because will not converge. Probably not enough information. 





