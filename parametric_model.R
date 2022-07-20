################################################################
####      PARAMETRIC SURVIVAL MODEL   ##########################
################################################################

rm(list = ls())

library(flexsurv) # using dev package from GitHub
library(MuMIn)
library(tidyr)
library(dplyr)
library(tidyverse)
library(survival)

ph <- readRDS('./data/RData/ph_Apr26_2022.Rds')
head(ph)

source("fxn_tidy_flexsurv.R")
source("flexsurvreg_fxns.R") # to make coefficients show up when using flexsurvreg with MuMIn

#source('MyFunctions.R') # create AICc table

global.model <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SIC_mean + speed3_max + dist_land + dist_pack + sd7 + ResidMass + wind_dir, 
                      dist = "exp", method = "Nelder-Mead", data = ph, cl = 0.95, na.action = "na.fail")

null.model <- flexsurvreg(Surv(tstart, tstop, migrate) ~ 1, dist = "exp", data = ph, cl = 0.95, na.action = "na.fail")

t <- dredge(global.model, beta = FALSE, evaluate = TRUE, rank = "AICc", m.lim = c(1,3))

t[1:10]
t[29]
t[2]

model.avg(t)  #model-averaged coefficients
sw(t) # variable weights

tt <- t[1:10,] # all model with AICc < 4

write.csv(tt, file = './data/derived-data/top_models.csv')

top <- get.models(t, subset = 1:2) # most parsimonious model (#2 in AICc ranking)
tidytop <- tidy.flexsurvreg(top)

write.csv(top, file = './data/derived-data/best_model.csv')

