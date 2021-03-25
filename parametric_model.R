################################################################
####      PARAMETRIC SURVIVAL MODEL   ##########################
################################################################

rm(list = ls())

library(flexsurv) # using dev package from GitHub
library(MuMIn)
library(tidyr)
library(dplyr)
library(tidyverse)

ph <- readRDS('./data/RData/ph_Mar12.Rds')

ph$SIC3 <- sqrt(ph$SICsq3)

#source('MyFunctions.R') # create AICc table

global.model <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SIC3 + speed3 + dist_land3 + dist_pack3 + sd7 + ResidMass + SIC3*speed3 + dir, 
                      dist = "exp", method = "Nelder-Mead", data = ph, na.action = "na.fail")

t <- dredge(global.model, beta = FALSE, evaluate = TRUE, rank = "AICc", m.lim = c(1,3))

tt <- t[1:10,]

fit_dir <- flexsurvreg(Surv(tstart, tstop, migrate) ~ dir, dist = "exp", data = ph, na.action = "na.fail")

fit <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SICsq3 + speed3 + dist_pack3, 
                   data = ph, dist = "exp", method = "Nelder-Mead", na.action = "na.fail")

write.csv(tt, file = './data/derived-data/top_models.csv')

top <- get.models(t, subset = 1:10)

# ------    CREATE NEW DATA TO PREDICT OVER ---------------- #

source("fxn_predict_flexsurv.R") # run script for predict function on flexsurv object

new_15 <- data.frame(speed3 = seq(0,20, length.out = 100), SICsq3 = 10000, dist_pack3 = 200, tstart = seq(1,100,1), tstop = seq(2, 101, 1)) # SIC at 15%

p <- predict.flexsurvreg(fit, newdata = new_15, type = "survival", na.action = "na.pass", times = 1)
p$index <- seq(0,20,length.out = 100)

qplot(data = p, x = index, y = .pred, geom = "line")

head(p)

colnames(p) <- c("pred_15", "windspeed")

new_30 <- data.frame(speed3 = seq(0,11, length.out = 100), SICsq3 = 900, dist_pack3 = 38.06, tstart = seq(1,100,1), tstop = seq(2, 101, 1))
p30 <- predict.flexsurvreg(fit, newdata = new_30, type = "response", na.action = "na.pass", times = 1)
head(p30)

p$pred_30 <- p30

colnames(p) <- c("pred_15", "ws", "pred_30")

ggplot(data = p, aes(x = ws, y = pred_15)) + 
  geom_line(color = "black") + 
  geom_line(aes(y = pred_30$.pred), color = "red")
  



         