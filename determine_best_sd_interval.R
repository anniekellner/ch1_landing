##############################################
###   STANDARD DEVIATION OF SIC ##############
##############################################

# https://stackoverflow.com/questions/24066085/rolling-standard-deviation-in-a-matrix-in-r

rm(list= ls())

library(zoo)
library(MuMIn)
library(survival)
library(dplyr)

load('cox_tdc.RData')
load('Cox_TDC.RData')

cox_tdc$sd3 <- rollapplyr(cox_tdc$SIC_mean, 3, sd, fill = 0) #sd over 3 days
cox_tdc$sd5 <- rollapplyr(cox_tdc$SIC_mean, 5, sd, fill = 0) #sd over 5 days
cox_tdc$sd7 <- rollapplyr(cox_tdc$SIC_mean, 7, sd, fill = 0) #sd over 7 days
cox_tdc$sd9 <- rollapplyr(cox_tdc$SIC_mean, 9, sd, fill = 0) #sd over 9 days
cox_tdc$sd11 <- rollapplyr(cox_tdc$SIC_mean, 11, sd, fill = 0) #sd over 11 days

head(cox_tdc)

# See which interval is best

# Functions

create_AICc_table <- function(aicc){ # AICc table function
  aicc$Model <- rownames(aicc)
  aicc$deltaAICc <- aicc$AICc - min(aicc$AICc) 
  aicc$L <- exp(-0.5*(aicc$deltaAICc))
  aicc$weight <- aicc$L/(sum(aicc$L))
  aicc$weight.pct <- aicc$weight*100
  aicc <- arrange(aicc, aicc$deltaAICc)
  return(aicc)
}

# Cox regression

sd3 <- coxph(Surv(tstart, tstop, migrate) ~ sd3, cluster = id, data = cox_tdc)
sd5 <- coxph(Surv(tstart, tstop, migrate) ~ sd5, cluster = id, data = cox_tdc)
sd7 <- coxph(Surv(tstart, tstop, migrate) ~ sd7, cluster = id, data = cox_tdc)
sd9 <- coxph(Surv(tstart, tstop, migrate) ~ sd9, cluster = id, data = cox_tdc)
sd11 <- coxph(Surv(tstart, tstop, migrate) ~ sd11, cluster = id, data = cox_tdc)

aicc <- AICc(sd3, sd5, sd7, sd9, sd11)

create_AICc_table(aicc) 

# sd 7 is best

cox_tdc <- cox_tdc %>%
  select(id:Windspeed, sd7)

save(cox_tdc, file = './data/RData/Cox_TDC.RData')

