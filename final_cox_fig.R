############################
###   COX PLOT    ##########
############################


rm(list = ls())

library(dplyr)
library(survival)
library(pammtools)
library(ggplot2)
library(tidyverse)
library(stringr)
library(zoo)

cox <- readRDS('./data/RData/cox_tdc_draft1.Rds')

fit <- coxph(Surv(tstart, tstop, migrate) ~  windspeed3 + SIC3, 
                      cluster = animal, data = cox)

svf <- survfit(Surv(tstart, tstop, migrate) ~ windspeed3 + SIC3, cluster = animal, data = cox)

tidyfit <- broom::tidy(fit)

tidy_survfit <- broom::tidy(svf)

# Extract windspeed and SICsq data from Surv object

covs <- attr(svf$strata, "names")
covs <-str_split(covs, ",")
covs <- data.frame(matrix(unlist(covs), nrow=length(covs), byrow=T))
colnames(covs) <- c("windspeed3", "SIC3")
covs <- covs %>%
  mutate_all(~gsub("windspeed3=", "", .)) %>%
  mutate_all(~gsub("SIC3=", "", .))

covs$windspeed3 <- as.numeric(covs$windspeed3)
covs$SIC3 <- as.numeric(covs$SIC3)

covs <- cbind(tidy_survfit, covs)
covs <- dplyr::select(covs, -strata)

new <- covs %>% 
  make_newdata(windspeed3 = seq_range(windspeed3, n = 100), SIC3 = c(50)) 

new_30 <- covs %>% 
  make_newdata(windspeed3 = seq_range(windspeed3, n = 100), SIC3 = c(30)) 

new_30 <- new_30 %>%
  mutate(lp = predict(fit, ., type = "lp", na.action = na.pass))

new_50 <- new %>%
  mutate(HR = predict(fit, ., type = "risk", na.action = na.pass))

ggplot(data = new_50) + 
  aes(x = windspeed3, y = HR) + 
  geom_line() + 
  xlab("Wind Speed (3-day mean)") + 
  ylab("Risk") + 
  theme_bw()

ggsave(filename = './figures/hr.png')


