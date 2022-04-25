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

ph <- readRDS('./data/RData/ph_Apr25_2022.Rds')

source("fxn_tidy_flexsurv.R")
source("flexsurvreg_fxns.R") # to make coefficients show up when using flexsurvreg with MuMIn

#source('MyFunctions.R') # create AICc table

global.model <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SIC_mean + speed3_max + dist_land + dist_pack + sd7 + ResidMass + wind_cos + wind_sin, 
                      dist = "exp", method = "Nelder-Mead", data = ph, cl = 0.95, na.action = "na.fail")

t <- dredge(global.model, beta = FALSE, evaluate = TRUE, rank = "AICc", m.lim = c(1,4), subset = (wind_sin | !wind_cos) && (wind_cos | ! wind_sin))

t[1]


model.avg(t)  #model-averaged coefficients

tt <- t[1:10,]

write.csv(tt, file = './data/derived-data/top_models.csv')

top <- get.models(t, subset = 1:2) # most parsimonious model (#2 in AICc ranking)
tidytop <- tidy.flexsurvreg(top)

write.csv(top, file = './data/derived-data/best_model.csv')

