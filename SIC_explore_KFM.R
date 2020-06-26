########################################################################################
########      EXPLORE SEA ICE CONCENTRATION FOR KNOWN FATE MODEL    ####################
########################################################################################

# McCall et al. 2016: nonlinear association between polar bears and sea ice. 
# SIC + SIC^2 
# see also Durner et al. 2004, 2006, 2009

# 2012 - NO DATA FOR JUNE - JULY 1. WILL HAVE TO DO STAGGERED ENTRY FOR 2012 BEARS

# 6/27/2020: REVERSE 0 AND 1 AND SEE WHAT HAPPENS


rm(list = ls())

library(dplyr)
library(raster)
library(MuMIn)

# Load data

load('KFM.RData')
load('SIC_KFM.RData')

SIC <- combine
rm(combine)

# add variable for "on.ice"
SIC$on.ice <- ifelse(SIC$swim == 1 | SIC$land == 1, 0, 1)

# Create quadratic SIC term

SIC$SICsq <- SIC$SIC_30m_me + (SIC$SIC_30m_me)^2


# logistic regression - mean SIC 30km radius vs. land or ice

fit_mean <- glm(on.ice ~ SIC_30m_me, data = SIC, family = binomial())
summary(fit_mean)

fit_max <- glm(on.ice ~ SIC_30m_max, data = SIC, family = binomial())
summary(fit_max)

fit_min <- glm(on.ice ~ SIC_30m_min, data = SIC, family = binomial())
summary(fit_min)

fit_sq <- glm(on.ice ~ SICsq, data = SIC, family = binomial())

aicc <- AICc(fit_mean, fit_max, fit_min, fit_sq)

# Create AIC table to compare models

create_AICc_table <- function(aicc){
  aicc$deltaAIC <- aicc$AIC - min(aicc$AIC) 
  aicc$weight <- exp(-1/2*(aicc$deltaAIC))/sum(exp(-1/2*(aicc$deltaAIC)))
  aicc$weight.pct <- aicc$weight*100
  return(aicc)
}

create_AICc_table(aicc)


ggplot(logreg, aes(mean_val, swim)) + geom_point() +
  scale_x_reverse() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
