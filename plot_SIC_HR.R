###########################################################################
##    PLOT: SEA ICE CONCENTRATION WHEN WIND AND DISTANCE NOT INCLUDED  ####
###########################################################################

library(flexsurv)
library(ggplot2)
library(data.table)
library(dplyr)

rm(list = ls())

source("fxn_predict_flexsurv.R") # function to use 'predict' with flexsurv object

ph <- readRDS('./data/RData/ph_Dec7.Rds')

fit <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SIC_mean + speed3_max_mean, 
                   data = ph, dist = "exp", method = "Nelder-Mead", na.action = "na.fail")

# Create new dataframe

set.seed(13)

new <- data.frame(SIC_mean = seq(0,100, by = 5), speed3_max_mean = mean(ph$speed3_max_mean))

# To compare survival rates run 'survival' model 

p <- predict.flexsurvreg(fit, newdata = new, type = "hazard",times = 1, na.action = "na.pass")

SIC <- seq(0,100, by = 5)


#p <- predict.flexsurvreg(fit, newdata = new, type = "survival",times = 1, na.action = "na.pass")

p$SIC <- SIC

p <- p %>%
  mutate(proportion_migrating = .pred * 100) # justification for multiplying by 100: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5388384/

ggplot(data = p, aes(x = SIC, y = proportion_migrating)) + 
  geom_path() + 
  geom_point(size = 3) +
  scale_x_reverse(breaks = c(100,90,80,70,60,50,40,30,20,10,0)) +
  #ylim(0,6) +
  xlab("Sea Ice Concentration (%)") + 
  ylab("Percentage of bears leaving ice per day") +
  theme_bw(base_size = 12)

# Hazard - same as flipside of survival

p$.pred <- p$.pred*100

ggplot(data = p, aes(x = SIC, y = proportion_migrating)) + 
  geom_point(size = 3) +
  scale_x_continuous(breaks = seq(0,100, 10), expand = c(0,0)) +
  #scale_x_discrete(breaks = c(100,90,80,70,60,50,40,30,20,10,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,12)) +
  coord_cartesian(clip = "off") + # so that points don't fall off plot
  xlab("Sea Ice Concentration (%)") + 
  ylab("Hazard Rate (% per day)") +
  theme_classic(base_size = 20)

ggsave('figures/SIC_HR_Cherryscale.png')
