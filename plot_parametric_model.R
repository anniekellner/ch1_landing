##############################################
#   PLOTS FOR PARAMETRIC SURVIVAL MODEL   ####
##############################################

library(flexsurv)
library(ggplot2)
library(data.table)

rm(list = ls())

source("fxn_predict_flexsurv.R") # function to use 'predict' with flexsurv object
source("fxn_tidy_flexsurv.R")


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

ws <- seq(0,15, length.out = 100)
reps <- rep(ws, each = 1309)

# Add to unnested dataframe

un0$ws <- reps
un15$ws <- reps
un30$ws <- reps
un50$ws <- reps
un100$ws <- reps

s0 <- un0 %>%
  group_by(ws) %>%
  slice_head %>%
  rename(pred0 = .pred)

qplot(data = s0, x = ws, y = pred0, geom = "line")

s15 <- un15 %>%
  group_by(ws) %>%
  slice_head %>%
  rename(pred15 = .pred)

s30 <- un30 %>%
  group_by(ws) %>%
  slice_head %>%
  rename(pred30 = .pred)

s50 <- un50 %>%
  group_by(ws) %>%
  slice_head %>%
  rename(pred50 = .pred)

s100 <- un100 %>%
  group_by(ws) %>%
  slice_head %>%
  rename(pred100 = .pred)

x <- left_join(s0, s15)
x <- x %>%
  left_join(s30)

x <- x %>%
  left_join(s50)

x <- x %>%
  left_join(s100)

x.long <- x %>%
  pivot_longer(cols = starts_with("pred"),
               names_to = "SIC",
               values_to = "HR")

# Reorder so legend appears in correct order in plot

x.long$SIC <- factor(x.long$SIC, levels = c("pred0", "pred15", "pred30", "pred50", "pred100"), labels = c("0", "15", "30", "50", "100"))
x.long$HR <- x.long$HR * 100

# Plot

ggplot(data = x.long, aes(x = ws, y = HR, col = SIC)) + 
         geom_line() + 
  labs(color = "Sea Ice Concentration") + 
  xlab("Wind Speed") + 
  ylab("Hazard Rate (% per day)\n") +
  theme_bw()





