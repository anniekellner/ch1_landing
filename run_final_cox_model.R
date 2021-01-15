##########################################
##    RUNNING THE FINAL COX MODEL ########
##########################################

library(survival)
library(MuMIn)
library(zoo)

rm(list = ls())

source('MyFunctions.R')

cox <- readRDS('./data/RData/cox_tdc.Rds')

# Single variables

sic <- coxph(Surv(tstart, tstop, migrate) ~ SICsq, cluster = animal, data = cox)
sd <- coxph(Surv(tstart, tstop, migrate) ~ sd7, cluster = animal, data = cox)
dist_ice <- coxph(Surv(tstart, tstop, migrate) ~ dist_to_ice, cluster = animal, data = cox)
dist_land <- coxph(Surv(tstart, tstop, migrate) ~ Distance_to_land, cluster = animal, data = cox)
windspeed <- coxph(Surv(tstart, tstop, migrate) ~ Windspeed, cluster = animal, data = cox)
pland <- coxph(Surv(tstart, tstop, migrate) ~ pland, cluster = animal, data = cox)
te <- coxph(Surv(tstart, tstop, migrate) ~ te, cluster = animal, data = cox)

aicc <- AICc(sic, sd, dist_ice, dist_land, windspeed, pland, te)

# Combinations

pland <- coxph(Surv(tstart, tstop, migrate) ~ pland, cluster = animal, data = cox)
windspeed <- coxph(Surv(tstart, tstop, migrate) ~ Windspeed, cluster = animal, data = cox)
te <- coxph(Surv(tstart, tstop, migrate) ~ te, cluster = animal, data = cox)
pland_pls_windspeed <- coxph(Surv(tstart, tstop, migrate) ~ pland + Windspeed, cluster = animal, data = cox)
pland_pls_te <- coxph(Surv(tstart, tstop, migrate) ~ pland + te, cluster = animal, data = cox)
windspeed_pls_te <- coxph(Surv(tstart, tstop, migrate) ~ te + Windspeed, cluster = animal, data = cox)
pland_times_windspeed <- coxph(Surv(tstart, tstop, migrate) ~ pland*Windspeed, cluster = animal, data = cox)
pland_times_te <- coxph(Surv(tstart, tstop, migrate) ~ te*pland, cluster = animal, data = cox)
pland_pls_windspeed_pls_te <- coxph(Surv(tstart, tstop, migrate) ~ pland + Windspeed + te, cluster = animal, data = cox)
te_times_windspeed <- coxph(Surv(tstart, tstop, migrate) ~ te*Windspeed, cluster = animal, data = cox)

aicc <- AICc(windspeed, pland, te, pland_pls_te, pland_pls_windspeed, pland_pls_windspeed_pls_te, pland_times_te, pland_times_windspeed, te_times_windspeed)

create_AICc_table(aicc)

# With 3-day moving window

cox <- cox %>%
  mutate(pland3 = rollmean(pland, 3, fill = NA, align = "right")) %>%
  mutate(windspeed3 = rollmean(Windspeed, 3, fill = NA, align = "right")) %>%
  mutate(te3 = rollmean(te, 3, fill = NA, align = "right")) %>%
  mutate(dist_land3 = rollmean(Distance_to_land, 3, fill = NA, align = "right")) %>%
  mutate(dist_ice3 = rollmean(dist_to_ice, 3, fill = NA, align = "right")) 


# Makes sense to use 3-day moving window for all variables for ease of interpretation

# FINAL: ( all 3-day moving window except rate (sd))

# pland

ice <- coxph(Surv(tstart, tstop, migrate) ~ pland3, cluster = animal, data = cox)

ice_pls_wind <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3, cluster = animal, data = cox)
ice_pls_distland <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + dist_land3 , cluster = animal, data = cox)
ice_pls_distpack <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + dist_ice3, cluster = animal, data = cox)
ice_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + sd7, cluster = animal, data = cox)

ice_pls_wind_pls_distland <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3 + dist_land3, cluster = animal, data = cox)
ice_pls_wind_pls_distpack <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3 + dist_ice3, cluster = animal, data = cox)
ice_pls_wind_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3 + sd7, cluster = animal, data = cox)

ice_pls_wind_pls_distland_pls_distpack <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3 + dist_land3 + dist_ice3, cluster = animal, data = cox)
ice_pls_wind_pls_distland_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3 + dist_land3 + sd7, cluster = animal, data = cox)

ice_pls_wind_pls_distland_pls_distpack_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3 + dist_land3 + dist_ice3 + sd7, cluster = animal, data = cox)

ice_pls_distland_pls_distpack <- ice_pls_distland <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + dist_land3 + dist_ice3 , cluster = animal, data = cox)

ice_x_wind <- coxph(Surv(tstart, tstop, migrate) ~ pland3 * windspeed3, cluster = animal, data = cox)
ice_x_wind_pls_distland <- coxph(Surv(tstart, tstop, migrate) ~ pland3 * windspeed3 + dist_land3, cluster = animal, data = cox)
ice_x_wind_pls_distpack <- coxph(Surv(tstart, tstop, migrate) ~ pland3 * windspeed3 + dist_ice3, cluster = animal, data = cox)
ice_x_wind_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ pland3 * windspeed3 + rate, cluster = animal, data = cox)
ice_x_wind_pls_distland_pls_distpack <- coxph(Surv(tstart, tstop, migrate) ~ pland3 * windspeed3 + dist_land3 + dist_ice3, cluster = animal, data = cox)
ice_x_wind_pls_distland_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ pland3 * windspeed3 + dist_land3 + sd7, cluster = animal, data = cox)
ice_x_wind_pls_distland_pls_distpack_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ pland3 * windspeed3 + dist_land3 + dist_ice3 + sd7, cluster = animal, data = cox)

# Total edge

edge <- coxph(Surv(tstart, tstop, migrate) ~ te3, cluster = animal, data = cox)

edge_pls_wind <- coxph(Surv(tstart, tstop, migrate) ~ te3 + windspeed3, cluster = animal, data = cox)
edge_pls_distland <- coxph(Surv(tstart, tstop, migrate) ~ te3 + dist_land3 , cluster = animal, data = cox)
edge_pls_distpack <- coxph(Surv(tstart, tstop, migrate) ~ te3 + dist_ice3, cluster = animal, data = cox)
edge_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ te3 + sd7, cluster = animal, data = cox)

edge_pls_wind_pls_distland <- coxph(Surv(tstart, tstop, migrate) ~ te3 + windspeed3 + dist_land3, cluster = animal, data = cox)
edge_pls_wind_pls_distpack <- coxph(Surv(tstart, tstop, migrate) ~ te3 + windspeed3 + dist_ice3, cluster = animal, data = cox)
edge_pls_wind_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ te3 + windspeed3 + sd7, cluster = animal, data = cox)

edge_pls_wind_pls_distland_pls_distpack <- coxph(Surv(tstart, tstop, migrate) ~ te3 + windspeed3 + dist_land3 + dist_ice3, cluster = animal, data = cox)
edge_pls_wind_pls_distland_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ te3 + windspeed3 + dist_land3 + sd7, cluster = animal, data = cox)

edge_pls_wind_pls_distland_pls_distpack_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ te3 + windspeed3 + dist_land3 + dist_ice3 + sd7, cluster = animal, data = cox)

edge_pls_distland_pls_distpack <- edge_pls_distland <- coxph(Surv(tstart, tstop, migrate) ~ te3 + dist_land3 + dist_ice3 , cluster = animal, data = cox)

edge_x_wind <- coxph(Surv(tstart, tstop, migrate) ~ te3 * windspeed3, cluster = animal, data = cox)
edge_x_wind_pls_distland <- coxph(Surv(tstart, tstop, migrate) ~ te3 * windspeed3 + dist_land3, cluster = animal, data = cox)
edge_x_wind_pls_distpack <- coxph(Surv(tstart, tstop, migrate) ~ te3 * windspeed3 + dist_ice3, cluster = animal, data = cox)
edge_x_wind_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ te3 * windspeed3 + rate, cluster = animal, data = cox)
edge_x_wind_pls_distland_pls_distpack <- coxph(Surv(tstart, tstop, migrate) ~ te3 * windspeed3 + dist_land3 + dist_ice3, cluster = animal, data = cox)
edge_x_wind_pls_distland_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ te3 * windspeed3 + dist_land3 + sd7, cluster = animal, data = cox)
edge_x_wind_pls_distland_pls_distpack_pls_rate <- coxph(Surv(tstart, tstop, migrate) ~ te3 * windspeed3 + dist_land3 + dist_ice3 + sd7, cluster = animal, data = cox)

# Single variables

wind <- coxph(Surv(tstart, tstop, migrate) ~ windspeed3, cluster = animal, data = cox)
distpack <- coxph(Surv(tstart, tstop, migrate) ~ dist_ice3, cluster = animal, data = cox)
distland <- coxph(Surv(tstart, tstop, migrate) ~ dist_land3, cluster = animal, data = cox)
rate <- coxph(Surv(tstart, tstop, migrate) ~ sd7, cluster = animal, data = cox)












aicc <- AICc(sic, sd, dist_ice, dist_land, windspeed, pland, te, te3, pland3, windspeed3)
create_AICc_table(aicc)

