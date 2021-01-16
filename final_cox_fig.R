############################
###   COX PLOT    ##########
############################

# 1/16/2021

rm(list = ls())

library(dplyr)
library(survival)
library(pammtools)
library(ggplot2)
library(tidyverse)
library(stringr)

cox <- readRDS('./data/RData/cox_tdc.Rds')

fit <- coxph(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3 + dist_land3, 
                      cluster = animal, data = cox)

surv_fit <- survfit(Surv(tstart, tstop, migrate) ~ pland3 + windspeed3 + dist_land3, 
                    cluster = animal, data = cox)

tidyfit <- broom::tidy(fit)

tidy_survfit <- broom::tidy(surv_fit)

# Extract windspeed and SICsq data from Surv object

covs <- attr(surv_fit$strata, "names")
covs <-str_split(covs, ",")
covs <- data.frame(matrix(unlist(covs), nrow=length(covs), byrow=T))
colnames(covs) <- c("Dist2land_3davg", "Windspeed_3davg")
covs <- covs %>%
  mutate_all(~gsub("Dist2land_3davg=", "", .)) %>%
  mutate_all(~gsub("Windspeed_3davg=", "", .))

covs$Windspeed_3davg <- as.numeric(covs$Windspeed_3davg)
covs$Dist2land_3davg <- as.numeric(covs$Dist2land_3davg)

covs <- cbind(tidy_survfit, covs)
covs <- dplyr::select(covs, -strata)

new <- covs %>% 
  make_newdata(Windspeed_3davg = seq_range(Windspeed_3davg, n = 100)) # holds all variables constant

new <- new %>%
  mutate(HR = predict(fit, ., type = "risk", na.action = na.pass))

ggplot(data = new) + 
  aes(x = Windspeed_3davg, y = HR) + 
  geom_line() + 
  xlab("Windspeed (3-day mean)") + 
  ylab("Hazard Ratio")

