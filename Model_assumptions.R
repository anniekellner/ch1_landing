################################################
##    ASSUMPTIONS - PH / EXPONENTIAL FORM   ####
################################################

rm(list = ls())

library(survival)
library(survminer)
library(flexsurv)
library(lubridate)
library(dplyr)
library(sf)

source('MyFunctions.R')

ph <- readRDS('./data/RData/ph_Mar25.Rds')

#ph$SIC3 <- sqrt(ph$SICsq3) # Do not use square of SIC for ease of interpretation
#ph$dist_land3 <- ph$dist_land3 * 1000 # For some reason had been divided by 1000 twice
#ph$dist_pack3 <- ph$dist_pack3* 1000

#saveRDS(ph, file = './data/RData/ph_Mar25.Rds') # SIC without being squared, multiplied distance metrics by 1000

global.model <- coxph(Surv(tstart, tstop, migrate) ~ SIC3 + speed3 + dist_land3 + dist_pack3 + sd7 + ResidMass + SIC3*speed3 + dir, 
                      cluster = animal, data = ph, na.action = "na.fail")

top <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SIC3 + speed3, 
                      dist = "exp", data = ph, na.action = "na.fail")

# Schoenfeld residuals: 
#insignificant values mean there is no relationship netween the cov and time, so PH assumption holds
#

sr <- cox.zph(global.model)

sr

ggcoxzph(sr)

# Dfbeta is a measurement of the estimated change in regression coefficient when each observation is deleted

ggcoxdiagnostics(global.model, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())

# Deviance residuals. 
# The deviance residual is a normalized transform of the martingale residual. 
# These residuals should be roughtly symmetrically distributed about zero with a standard deviation of 1.
# Positive values correspond to individuals that “died too soon” compared to expected survival times.
# Negative values correspond to individual that “lived too long”.
# Very large or small values are outliers, which are poorly predicted by the model.

ggcoxdiagnostics(global.model, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw()) # No huge values but most are negative

# Martingale residuals - did not use

fit.SIC3 <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SIC3, 
                        data = ph, dist = "exp", na.action = "na.fail")

mart <- residuals(fit.SIC3, type = "martingale")
plot(mart)

# Time as a covariate

t <- readRDS('./data/RData/land_bears_cutoff_after_swim.Rds')

t$ordinal <- yday(t$datetime)

t <- st_drop_geometry(t)

# ----  CREATE DF WITH DAILY AVERAGES  ------------- #

t <- t %>% # Compute daily average
  group_by(id, ordinal) %>%
  dplyr::summarise(
    first(animal), first(year), max(start.swim), first(ordinal)) %>%
  ungroup()

t <- t %>%
  group_by(id) %>%
  mutate(day = row_number()) 

colnames(t) <- c("id", "datetime", "animal", "year", "start.swim", "ordinal")

temp <- subset(t, start.swim == 1)

temp <- temp %>%
  dplyr::select(id, day, ordinal, start.swim, animal, year) 



baseline <- tmerge(temp, temp, id = id, migrate = event(day, start.swim), tstart = 1, tstop = day)

timetest <- tmerge(baseline, t, id = id, 
                   time = tdc(day, ordinal))


fit_time <- flexsurvreg(Surv(tstart, tstop, migrate) ~ time, data = timetest, dist = "exp")
