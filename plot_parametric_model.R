###############################################################################
#   PLOTS FOR PARAMETRIC SURVIVAL MODEL: WIND SPEED, SIC AND HAZARD RATE   ####
###############################################################################

library(flexsurv)
library(ggplot2)
library(data.table)
library(tidyr)
library(dplyr)

rm(list = ls())

source("fxn_predict_flexsurv.R") # function to use 'predict' with flexsurv object
source("fxn_tidy_flexsurv.R")

ph <- readRDS('./data/RData/ph_Dec7.Rds')

#min(ph$speed3_max_mean)
#max(ph$speed3_max_mean)


# Fit model

fit <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SIC_mean + speed3_max_mean, 
                   data = ph, dist = "exp", method = "Nelder-Mead", na.action = "na.fail")

# Create new dataframes

# With distance ro pack

new_1 <- data.frame(speed3_max_mean = seq(0,13, length.out = 100), SIC_mean = 1, dist_pack = mean(ph$dist_pack)) 
new_15 <- data.frame(speed3_max_mean = seq(0,13, length.out = 100), SIC_mean = 15, dist_pack = mean(ph$dist_pack)) 
new_30 <- data.frame(speed3_max_mean = seq(0,13, length.out = 100), SIC_mean = 30, dist_pack = mean(ph$dist_pack))
new_50 <- data.frame(speed3_max_mean = seq(0,13, length.out = 100), SIC_mean = 50, dist_pack = mean(ph$dist_pack))
new_100 <- data.frame(speed3_max_mean = seq(0,13, length.out = 100), SIC_mean = 100, dist_pack = mean(ph$dist_pack))

# WITHOUT distance to pack 

new_1 <- data.frame(speed3_max_mean = seq(0,13, length.out = 100), SIC_mean = 1) 
new_15 <- data.frame(speed3_max_mean = seq(0,13, length.out = 100), SIC_mean = 15) 
new_30 <- data.frame(speed3_max_mean = seq(0,13, length.out = 100), SIC_mean = 30)
new_50 <- data.frame(speed3_max_mean = seq(0,13, length.out = 100), SIC_mean = 50)
new_100 <- data.frame(speed3_max_mean = seq(0,13, length.out = 100), SIC_mean = 100)


p1 <- predict.flexsurvreg(fit, newdata = new_1, type = "hazard", times = 1, na.action = "na.pass")
p15 <- predict.flexsurvreg(fit, newdata = new_15, type = "hazard", times = 1, na.action = "na.pass")
p30 <- predict.flexsurvreg(fit, newdata = new_30, type = "hazard", times = 1, na.action = "na.pass")
p50 <- predict.flexsurvreg(fit, newdata = new_50, type = "hazard", times = 1, na.action = "na.pass")
p100 <- predict.flexsurvreg(fit, newdata = new_100, type = "hazard", times = 1, na.action = "na.pass")

# Assign windspeed to each new dataframe

ws <- seq(0,13, length.out = 100)

p1$ws <- ws
p15$ws <- ws
p30$ws <- ws
p50$ws <- ws
p100$ws <- ws


#qplot(data = p1, x = ws, y = .pred, geom = "line")

# Rename .pred column (survival prediction)

p1 <- rename(p1, pred1 = .pred)
p15 <- rename(p15, pred15 = .pred)
p30 <- rename(p30, pred30 = .pred)
p50 <- rename(p50, pred50 = .pred)
p100 <- rename(p100, pred100 = .pred)


x <- left_join(p1, p15)

x <- x %>%
  left_join(p30)

x <- x %>%
  left_join(p50)

x <- x %>%
  left_join(p100)

x.long <- x %>%
  pivot_longer(cols = starts_with("pred"),
               names_to = "SIC",
               values_to = "HR")

# Reorder so legend appears in correct order in plot

x.long$SIC <- factor(x.long$SIC, levels = c("pred1", "pred15", "pred30", "pred50", "pred100"), labels = c("1", "15", "30", "50", "100"))
#x.long$HR <- x.long$HR * 100 # Initial thinking was *100

#x.long$expHR <- exp(x.long$HR) # Assuming the hazard estimated by the predict.flexsurv corresponds to est and not exp(est)

# Plot

ggplot(data = x.long, aes(x = ws, y = HR, col = SIC)) + 
         geom_line(size = 1) + 
  scale_x_continuous(limits = c(0, 13), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,0.2), expand = c(0,0)) +
  labs(color = "Sea Ice Concentration") + 
  xlab("3-Day Mean Wind Speed (m/s)") + 
  ylab("Hazard") +
  theme_bw()





