##########################################
##    RUNNING THE FINAL COX MODEL ########
##########################################

library(survival)
library(MuMIn)
library(zoo)
library(tidyr)
library(dplyr)

rm(list = ls())

cox <- readRDS('./data/RData/cox_tdc_draft1.Rds')



# Add mean values for missing values so don't lose valuable data

cox <- cox %>%
  filter(!(year == 2005)) %>%
  replace_na(list(ResidMass = -8.384, sd7_SIC = 2, sd7_pland = 0))

#apply(is.na(cox),2, which)

#cox <- cox %>%
  #filter(!(year == 2005))

# 3-day moving window

cox <- cox %>%
  group_by(id) %>%
  mutate(SIC3 = rollmean(SIC, 3, fill = NA, align = "right")) %>%
  mutate(pland3 = rollmean(pland, 3, fill = NA, align = "right")) %>%
  mutate(windspeed3 = rollmean(windspeed, 3, fill = NA, align = "right")) %>%
  mutate(te3 = rollmean(te, 3, fill = NA, align = "right")) %>%
  mutate(dist_land3 = rollmean(dist_land, 3, fill = NA, align = "right")) %>%
  mutate(dist_ice3 = rollmean(dist_pack, 3, fill = NA, align = "right")) 

# Change units to km instead of m because effect size makes more sense

cox$dist_land3_km <- cox$dist_land3 / 1000
cox$dist_ice3_km <- cox$dist_ice3 / 1000

# Final form of dataframe

cox <- cox %>% select(-c(dist_land3, dist_ice3, dist_land_km))

saveRDS(cox, file = './data/RData/cox_tdc_draft1.Rds')


cox <- na.omit(cox) # NA values giving me grief. Removed for now. 

global.model <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + SIC3 + windspeed3 + te3 + dist_land3_km + dist_ice3_km + sd7_SIC + year + ResidMass, 
                      cluster = animal, data = cox, na.action = "na.fail")

t <- dredge(global.model, beta = FALSE, evaluate = TRUE, rank = "AICc", m.lim = c(2,3))


# From MuMIn: "Use of na.action = "na.omit" (R's default) or "na.exclude" in global.model must be avoided, 
#as it results with sub-models fitted to different data sets, if there are missing values. Error is thrown if it is detected.

tt <- t[1:10,]
tt

# Summarize top model

fit <- coxph(Surv(tstart, tstop, migrate) ~ windspeed3 + dist_land3_km, cluster = animal, data = cox)
summary(fit)

write.csv(tt, file = './data/derived-data/top_models.csv')
