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

#source('MyFunctions.R') # create AICc table

global.model <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SICsq3 + speed3 + dist_land3 + dist_pack3 + sd7 + ResidMass + SICsq3*speed3, 
                      dist = "exp", data = ph, method = "Nelder-Mead", na.action = "na.fail")

t <- dredge(global.model, beta = FALSE, evaluate = TRUE, rank = "AICc", m.lim = c(1,3))

tt <- t[1:10,]
tt

write.csv(tt, file = './data/derived-data/top_models.csv')

top <- get.models(t, subset = 1)





         