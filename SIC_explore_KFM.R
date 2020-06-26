########################################################################################
########      EXPLORE SEA ICE CONCENTRATION FOR KNOWN FATE MODEL    ####################
########################################################################################

# McCall et al. 2016: nonlinear association between polar bears and sea ice. 
# SIC + SIC^2 
# see also Durner et al. 2004, 2006, 2009

# 2012 - NO DATA FOR JUNE - JULY 1. WILL HAVE TO DO STAGGERED ENTRY FOR 2012 BEARS


rm(list = ls())

library(dplyr)
library(raster)

# Load data

load('KFM.RData')
load('SIC_KFM.RData')

SIC <- combine
rm(combine)

# add variable for "on.ice"
SIC$on.ice <- ifelse(SIC$swim == 1 | SIC$land == 1, 0, 1)


# logistic regression - mean SIC 30km radius vs. land or ice

fit_mean <- glm(on.ice ~ SIC_30m_me, data = SIC, family = binomial())
summary(fit_mean)

fit_max <- glm(on.ice ~ SIC_30m_max, data = SIC, family = binomial())
summary(fit_max)

fit_min <- glm(on.ice ~ SIC_30m_min, data = SIC, family = binomial())
summary(fit_min)

AIC(fit_mean, fit_max, fit_min)

# Use S. Carver's code to create AIC tables for myself (see below)

AIC <- function(model.list, model.names) {
  K=c(NA)
  neg2logL=c(NA)
  aic=c(NA)
  for(i in 1:length(model.list)){
    K[i]=length(coef(model.list[[i]]))#K
    neg2logL[i]=-2*logLik(mods[[i]])#-2Log(L)
    aic[i]=AIC(mods[[i]])#AIC
  }
  deltaAIC=aic-min(aic)
  weight=exp(-1/2*deltaAIC)/sum(exp(-1/2*deltaAIC))
  weightpct=weight*100
  table1=cbind(1:63,K,neg2logL,aic,deltaAIC,weight,weightpct)
  colnames(table1)=c("Model","K","-2LOG(L)","AIC","deltaAIC","Weight","Weight (%)")
  rownames(table1)=model.names
  table1=table1[order(table1[,4]),]
  table1=data.frame(table1)
  return(table1)
}



ggplot(logreg, aes(mean_val, swim)) + geom_point() +
  scale_x_reverse() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
