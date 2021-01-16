##########################################
##    RUNNING THE FINAL COX MODEL ########
##########################################

library(survival)
library(MuMIn)
library(zoo)
library(tidyr)

rm(list = ls())

cox <- readRDS('./data/RData/cox_tdc.Rds')

cox <- cox %>% 
  filter(!(year == 2005)) %>%
  select(-age) %>%
  replace_na(list(rm = -6.897, sd7 = 2, repro = 1))

mean(cox$rm, na.rm = TRUE) # -6.897

apply(is.na(cox),2, which)

#cox <- cox %>%
  #filter(!(year == 2005))

# 3-day moving window

cox <- cox %>%
  group_by(id) %>%
  mutate(pland3 = rollmean(pland, 3, fill = NA, align = "right")) %>%
  mutate(windspeed3 = rollmean(windspeed, 3, fill = NA, align = "right")) %>%
  mutate(te3 = rollmean(te, 3, fill = NA, align = "right")) %>%
  mutate(dist_land3 = rollmean(dist_land, 3, fill = NA, align = "right")) %>%
  mutate(dist_ice3 = rollmean(dist_pack, 3, fill = NA, align = "right")) 

cox <- na.omit(cox)

global.model <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3 + te3 + dist_land3 + dist_ice3 + sd7 + repro + rm + year + pland3*windspeed3, 
                      cluster = animal, data = cox, na.action = "na.fail") # no age because will not converge. Probably not enough information. 

t <- dredge(global.model, beta = FALSE, evaluate = TRUE, rank = "AICc")


#"Use of na.action = "na.omit" (R's default) or "na.exclude" in global.model must be avoided, 
#as it results with sub-models fitted to different data sets, if there are missing values. Error is thrown if it is detected.
