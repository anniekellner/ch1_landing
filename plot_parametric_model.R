##############################################
#   PLOTS FOR PARAMETRIC SURVIVAL MODEL   ####
##############################################

library(flexsurv)
library(ggplot2)
library(data.table)

source("fxn_predict_flexsurv.R") # function to use 'predict' with flexsurv object
source("fxn_tidy_flexsurv.R")

rm(list = ls())

ph <- readRDS('./data/RData/ph.Rds')

fit <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SICsq3 + speed3 + dist_pack3, 
                   data = ph, dist = "exp", method = "Nelder-Mead", na.action = "na.fail")

# Create new dataframes

new_0 <- data.frame(speed3 = seq(0,15, length.out = 100), SICsq3 = 0, dist_pack3 = 38.06) 
new_15 <- data.frame(speed3 = seq(0,15, length.out = 100), SICsq3 = 225, dist_pack3 = 38.06) 
new_30 <- data.frame(speed3 = seq(0,15, length.out = 100), SICsq3 = 900, dist_pack3 = 38.06)
new_50 <- data.frame(speed3 = seq(0,15, length.out = 100), SICsq3 = 2500, dist_pack3 = 38.06)
new_100 <- data.frame(speed3 = seq(0,15, length.out = 100), SICsq3 = 10000, dist_pack3 = 38.06)

p0 <- predict.flexsurvreg(fit, newdata = new_0, type = "hazard", na.action = "na.pass")
p15 <- predict.flexsurvreg(fit, newdata = new_15, type = "hazard", na.action = "na.pass")
p30 <- predict.flexsurvreg(fit, newdata = new_30, type = "hazard", na.action = "na.pass")
p50 <- predict.flexsurvreg(fit, newdata = new_50, type = "hazard", na.action = "na.pass")
p100 <- predict.flexsurvreg(fit, newdata = new_100, type = "hazard", na.action = "na.pass")

un0 <- unnest(p0, cols = c(.pred))
un15 <- unnest(p15, cols = c(.pred))
un30 <- unnest(p30, cols = c(.pred))
un50 <- unnest(p50, cols = c(.pred))
un100 <- unnest(p100, cols = c(.pred))



# Assign windspeed to each series of 1309 observations

ws <- seq(0,15, length.out = )
reps <- rep(ws, each = 1309)

un$ws <- reps

s <- un %>%
  group_by(ws, .time) %>%
  distinct()

w <- s %>%
  group_by(ws) %>%
  summarise(s = mean(.pred))

qplot(data = s, x = .time, y = .pred, geom = "line")

# Add columns to predict dataframe for different SIC's

p0 <- predict.flexsurvreg(fit, newdata = new_0, type = "survival", na.action = "na.pass", times = 1)
p30 <- predict.flexsurvreg(fit, newdata = new_30, type = "survival", na.action = "na.pass", times = 1)
p50 <- predict.flexsurvreg(fit, newdata = new_50, type = "survival", na.action = "na.pass", times = 1)
p100 <- predict.flexsurvreg(fit, newdata = new_100, type = "survival", na.action = "na.pass", times = 1)
 
p$p0 <- p0[,2]
p$p30 <- p30[,2]
p$p50 <- p50[,2]
p$p100 <- p100[,2]

test <- p %>%
  mutate(p15_cum = .pred*lag(.pred))

# Plot

ggplot(data = p, aes(x = index)) + 
  geom_line(aes(y = .pred), color = "red")




