###########################################################################
##    PLOT: SEA ICE CONCENTRATION WHEN WIND AND DISTANCE NOT INCLUDED  ####
###########################################################################

library(flexsurv)
library(ggplot2)
library(data.table)
library(dplyr)

rm(list = ls())

source("fxn_predict_flexsurv.R") # function to use 'predict' with flexsurv object

ph <- readRDS('./data/RData/ph_Mar26.Rds')

fit <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SIC3, 
                   data = ph, dist = "exp", method = "Nelder-Mead", na.action = "na.fail")

# Create new dataframe

set.seed(13)

new <- data.frame(SIC3 = seq(0,100, by = 5))

p <- predict.flexsurvreg(fit, newdata = new, type = "survival",times = 1, na.action = "na.pass")

SIC <- seq(0,100, by = 5)


# To compare survival rates run 'survival' model 

p <- predict.flexsurvreg(fit, newdata = new, type = "survival",times = 1, na.action = "na.pass")

p$SIC <- SIC

p <- p %>%
  mutate(proportion_migrating = (1 - .pred) * 100)

ggplot(data = p, aes(x = SIC, y = proportion_migrating)) + 
  geom_path() + 
  geom_point(size = 3) +
  scale_x_reverse(breaks = c(100,90,80,70,60,50,40,30,20,10,0)) +
  ylim(0,6) +
  xlab("Sea Ice Concentration (%)") + 
  ylab("Percentage of bears leaving ice per day") +
  theme_bw(base_size = 12)

ggsave('figures/SIC_HR.png')