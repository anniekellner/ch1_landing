#################################################################
###     REVISE PARAMETRIC MODEL PLOT SO SEA ICE CONCENTRATION ###
###                   IS ON X-AXIS                            ###
##################################################################

library(flexsurv)
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)

rm(list = ls())

source("fxn_predict_flexsurv.R") # function to use 'predict' with flexsurv object
source("fxn_tidy_flexsurv.R")

ph <- readRDS('./data/RData/ph_Dec7.Rds')

# Fit model

fit <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SIC_mean + speed3_max_mean, 
                   data = ph, dist = "exp", method = "Nelder-Mead", na.action = "na.fail")

# Create new dataframe

new_0 <- data.frame(SIC_mean = seq(0,13, length.out = 100), speed3_max_mean = 0) 
new_3 <- data.frame(SIC_mean = seq(0,13, length.out = 100), speed3_max_mean = 3) 
new_6 <- data.frame(SIC_mean = seq(0,13, length.out = 100), speed3_max_mean = 6)
new_9 <- data.frame(SIC_mean = seq(0,13, length.out = 100), speed3_max_mean = 9)
new_12 <- data.frame(SIC_mean = seq(0,13, length.out = 100), speed3_max_mean = 12)

# Predict over new df's

p0 <- predict.flexsurvreg(fit, newdata = new_0, type = "hazard", times = 1, na.action = "na.pass")
p3 <- predict.flexsurvreg(fit, newdata = new_3, type = "hazard", times = 1, na.action = "na.pass")
p6 <- predict.flexsurvreg(fit, newdata = new_6, type = "hazard", times = 1, na.action = "na.pass")
p9 <- predict.flexsurvreg(fit, newdata = new_9, type = "hazard", times = 1, na.action = "na.pass")
p12 <- predict.flexsurvreg(fit, newdata = new_12, type = "hazard", times = 1, na.action = "na.pass")

# Assign SIC to each new dataframe

SIC <- seq(0,100, length.out = 100)

p0$SIC <- SIC
p3$SIC <- SIC
p6$SIC <- SIC
p9$SIC <- SIC
p12$SIC <- SIC

# Rename prediction column

p0 <- rename(p0, pred0 = .pred)
p3 <- rename(p3, pred3 = .pred)
p6 <- rename(p6, pred6 = .pred)
p9 <- rename(p9, pred9 = .pred)
p12 <- rename(p12, pred12 = .pred)

# Join into single df

x <- left_join(p0, p3)

x <- x %>%
  left_join(p6)

x <- x %>%
  left_join(p9)

x <- x %>%
  left_join(p12)

x.long <- x %>%
  pivot_longer(cols = starts_with("pred"),
               names_to = "Windspeed",
               values_to = "HR")

x.long$Windspeed <- factor(x.long$Windspeed, levels = c("pred0", "pred3", "pred6", "pred9", "pred12"), labels = c("0", "3", "6", "9", "12"))

ggplot(data = x.long, aes(x = SIC, y = HR, col = Windspeed)) + 
  geom_line(size = 1) + 
  scale_x_reverse(breaks = c(100, 75, 50, 25, 0)) +
  labs(color = "3-day average max windspeed (m/s)") + 
  xlab("Sea ice concentration (%)") + 
  ylab("Hazard Rate") +
  theme_classic() +
  theme(legend.position = "none")

ggsave('./figures/SIC_wind_interaction.svg')
