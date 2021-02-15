################################################################
####      PARAMETRIC SURVIVAL MODEL   ##########################
################################################################

rm(list = ls())

library(flexsurv) # using dev package from GitHub
library(MuMIn)
library(zoo)
library(tidyr)
library(dplyr)
library(tidyverse)

ph <- readRDS('./data/RData/ph.Rds')

source('MyFunctions.R') # create AICc table

# Check SIC and make sure the models aren't crazy different when using 3-day moving window
# Exclude year because do not have big enough sample size to include year as a factor 


SIC <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SICsq3, data = ph, dist = "exp")
speed <- flexsurvreg(Surv(tstart, tstop, migrate) ~ speed3, data = ph, dist = "exp")
land <- flexsurvreg(Surv(tstart, tstop, migrate) ~ dist_land3, data = ph, dist = "exp")
pack <- flexsurvreg(Surv(tstart, tstop, migrate) ~ dist_pack3, data = ph, dist = "exp")
sd <- flexsurvreg(Surv(tstart, tstop, migrate) ~ sd7, data = ph, dist = "exp")
#year <- flexsurvreg(Surv(tstart, tstop, migrate) ~ year, data = ph, dist = "exp")

SIC_speed <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SICsq3 + speed3, data = ph, dist = "exp")
SIC_distland <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SICsq3 + dist_land3, data = ph, dist = "exp")
SIC_distpack <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SICsq3 + dist_pack3, data = ph, dist = "exp")
#SIC_year <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SICsq3 + year, data = ph, dist = "exp")

speed_land <- flexsurvreg(Surv(tstart, tstop, migrate) ~ speed3 + dist_land3, data = ph, dist = "exp")
speed_pack <- flexsurvreg(Surv(tstart, tstop, migrate) ~ speed3 + dist_pack3, data = ph, dist = "exp")
speed_sd <- flexsurvreg(Surv(tstart, tstop, migrate) ~ speed3 + sd7, data = ph, dist = "exp")
#speed_year <- flexsurvreg(Surv(tstart, tstop, migrate) ~ speed3 + year, data = ph, dist = "exp")

land_pack <- flexsurvreg(Surv(tstart, tstop, migrate) ~ dist_land3 + dist_pack3, data = ph, dist = "exp")
#land_year <- flexsurvreg(Surv(tstart, tstop, migrate) ~ dist_land3 + year, data = ph, dist = "exp")
land_sd <- flexsurvreg(Surv(tstart, tstop, migrate) ~ dist_land3 + sd7, data = ph, dist = "exp")
#land_year <- flexsurvreg(Surv(tstart, tstop, migrate) ~ dist_land3 + year, data = ph, dist = "exp")

pack_sd <- flexsurvreg(Surv(tstart, tstop, migrate) ~ dist_pack3 + sd7, data = ph, dist = "exp")
#pack_year <- flexsurvreg(Surv(tstart, tstop, migrate) ~ dist_pack3 + year, data = ph, dist = "exp")

#sd_year <- flexsurvreg(Surv(tstart, tstop, migrate) ~ sd7 + year, data = ph, dist = "exp")

aicc <- AICc(SIC, SIC_distland, SIC_distpack, SIC_speed, speed, speed_land, speed_pack, speed_sd, land, land_pack, land_sd, pack, pack_sd, sd)

create_AICc_table(aicc)

# ----- COX PROPORTIONAL HAZARD ---------------------------------------------------------------------------------------- #

SIC <- coxph(Surv(tstart, tstop, migrate) ~ SICsq3, data = ph, cluster = id)
speed <- coxph(Surv(tstart, tstop, migrate) ~ speed3, data = ph, cluster = id)
land <- coxph(Surv(tstart, tstop, migrate) ~ dist_land3, data = ph, cluster = id)
pack <- coxph(Surv(tstart, tstop, migrate) ~ dist_pack3, data = ph, cluster = id)
sd <- coxph(Surv(tstart, tstop, migrate) ~ sd7, data = ph, cluster = id)
#year <- coxph(Surv(tstart, tstop, migrate) ~ year, data = ph, cluster = id)

SIC_speed <- coxph(Surv(tstart, tstop, migrate) ~ SICsq3 + speed3, data = ph, cluster = id)
SIC_distland <- coxph(Surv(tstart, tstop, migrate) ~ SICsq3 + dist_land3, data = ph, cluster = id)
SIC_distpack <- coxph(Surv(tstart, tstop, migrate) ~ SICsq3 + dist_pack3, data = ph, cluster = id)
#SIC_year <- coxph(Surv(tstart, tstop, migrate) ~ SICsq3 + year, data = ph, cluster = id)

speed_land <- coxph(Surv(tstart, tstop, migrate) ~ speed3 + dist_land3, data = ph, cluster = id)
speed_pack <- coxph(Surv(tstart, tstop, migrate) ~ speed3 + dist_pack3, data = ph, cluster = id)
speed_sd <- coxph(Surv(tstart, tstop, migrate) ~ speed3 + sd7, data = ph, cluster = id)
#speed_year <- coxph(Surv(tstart, tstop, migrate) ~ speed3 + year, data = ph, cluster = id)

land_pack <- coxph(Surv(tstart, tstop, migrate) ~ dist_land3 + dist_pack3, data = ph, cluster = id)
#land_year <- coxph(Surv(tstart, tstop, migrate) ~ dist_land3 + year, data = ph, cluster = id)
land_sd <- coxph(Surv(tstart, tstop, migrate) ~ dist_land3 + sd7, data = ph, cluster = id)
#land_year <- coxph(Surv(tstart, tstop, migrate) ~ dist_land3 + year, data = ph, cluster = id)

pack_sd <- coxph(Surv(tstart, tstop, migrate) ~ dist_pack3 + sd7, data = ph, cluster = id)

speed_land
