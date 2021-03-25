################################################
##    ASSUMPTIONS - PH / EXPONENTIAL FORM   ####
################################################

rm(list = ls())

library(survival)
library(survminer)

ph <- readRDS('./data/RData/ph_Mar12.Rds')

ph$SIC3 <- sqrt(ph$SICsq3) # Do not use square of SIC for ease of interpretation
ph$dist_land3 <- ph$dist_land3 * 1000 # For some reason had been divided by 1000 twice
ph$dist_pack3 <- ph$dist_pack3* 1000

global.model <- coxph(Surv(tstart, tstop, migrate) ~ SIC3 + speed3 + dist_land3 + dist_pack3 + sd7 + ResidMass + SIC3*speed3 + dir, 
                      cluster = animal, data = ph, na.action = "na.fail")

# Schoenfeld residuals: 
#insignificant values mean there is no relationship netween the cov and time, so PH assumption holds
#

sr <- cox.zph(global.model)

sr

ggcoxzph(sr)

# Dfbeta is a measurement of the estimated change in regression coefficient when each observation is deleted

ggcoxdiagnostics(global.model, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())

