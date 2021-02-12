################################################################
####      PARAMETRIC SURVIVAL MODEL   ##########################
################################################################

rm(list = ls())

library(flexsurv)
library(MuMIn)
library(zoo)
library(tidyr)
library(dplyr)
library(tidyverse)

mydata <- readRDS('./data/RData/cox_tdc_draft1.Rds')

# Add functionality for flexsurvreg so acts similarly to survreg

nobs.flexsurvreg <- function(object, ...) {object$N}

coefTable.flexsurvreg <- function(model, ...) {
  x <- as.data.frame(model$res[,c("est","se")])
  colnames(x) <- c("Estimate", "Std. Error")
  x
}


tidy(exp)

# AICc

global.model <- flexsurvreg(Surv(tstart, tstop, migrate) ~ pland3 + SIC3 + windspeed3 + te3 + dist_land3_km + dist_ice3_km + sd7_SIC + year + ResidMass, 
                      data = mydata, dist = "exp", na.action = "na.fail")

t <- dredge(global.model, beta = FALSE, evaluate = TRUE, rank = "AICc", m.lim = c(2,3))

tt <- t[1:10,]
tt

exp <- flexsurvreg(Surv(tstart, tstop, migrate) ~ windspeed3 + dist_land3_km, data = mydata, dist = "exp")

coefTable(exp)
tidy(exp)



AICc(exp)




