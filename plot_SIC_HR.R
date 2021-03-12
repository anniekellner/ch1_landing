##############################################################
##    PLOT: SEA ICE CONCENTRATION WHEN WIND HELD AT MEAN  ####
##############################################################

library(flexsurv)
library(ggplot2)
library(data.table)

rm(list = ls())

source("fxn_predict_flexsurv.R") # function to use 'predict' with flexsurv object

ph <- readRDS('./data/RData/ph.Rds')

fit <- flexsurvreg(Surv(tstart, tstop, migrate) ~ SICsq3 + speed3 + dist_pack3, 
                   data = ph, dist = "exp", method = "Nelder-Mead", na.action = "na.fail")

# Create new dataframe

mean(ph$speed3)

new <- data.frame(SICsq3 = seq(0,10000, length.out = 20), speed3 = 3.38, dist_pack3 = 38.06)

p <- predict.flexsurvreg(fit, newdata = new, type = "hazard", na.action = "na.pass")
p <- unnest(p, cols = c(.pred))

# Assign SICsq to each series of 1309 observations

SIC <- seq(0,100, length.out = 20)
reps <- rep(SIC, each = 1309)

# Add to unnested dataframe

p$SIC <- reps

p <- p %>%
  group_by(SIC) %>%
  slice_head 

p$.pred <- p$.pred * 100

ggplot(data = p, aes(x = SIC, y = .pred)) + 
  geom_line() + 
  geom_point() +
  scale_x_reverse(breaks = c(100,90,80,70,60,50,40,30,20,10,0)) +
  xlab("Sea Ice Concentration (%)") + 
  ylab("Hazard Rate (% per day)\n") +
  theme_bw()

ggsave('figures/SIC_HR.png')


