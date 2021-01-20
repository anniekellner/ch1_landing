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
library(zoo)

cox <- readRDS('./data/RData/cox_tdc.Rds')

cox <- cox %>%
  group_by(id) %>%
  mutate(pland3 = rollmean(pland, 3, fill = NA, align = "right")) %>%
  mutate(windspeed3 = rollmean(windspeed, 3, fill = NA, align = "right")) %>%
  mutate(te3 = rollmean(te, 3, fill = NA, align = "right")) %>%
  mutate(dist_land3 = rollmean(dist_land, 3, fill = NA, align = "right")) %>%
  mutate(dist_ice3 = rollmean(dist_pack, 3, fill = NA, align = "right")) 

fit <- coxph(Surv(tstart, tstop, migrate) ~  windspeed3 + dist_land3, 
                      cluster = animal, data = cox)

fit2 <- coxph(Surv(tstart, tstop, migrate) ~ windspeed3 + dist_land3, 
              cluster = animal, data = cox)

svf <- survfit(Surv(tstart, tstop, migrate) ~ windspeed3 + dist_land3, cluster = animal, data = cox)

svf <- survfit(Surv(tstart, tstop, migrate) ~ pland3 + dist_land3, cluster = animal, data = cox)

tidyfit <- broom::tidy(fit)

tidy_survfit <- broom::tidy(svf)

# Extract windspeed and SICsq data from Surv object

covs <- attr(svf$strata, "names")
covs <-str_split(covs, ",")
covs <- data.frame(matrix(unlist(covs), nrow=length(covs), byrow=T))
colnames(covs) <- c("windspeed3", "dist_land3")
covs <- covs %>%
  mutate_all(~gsub("windspeed3=", "", .)) %>%
  mutate_all(~gsub("dist_land3=", "", .))

covs$windspeed3 <- as.numeric(covs$windspeed3)
covs$dist_land3 <- as.numeric(covs$dist_land3)

covs <- cbind(tidy_survfit, covs)
covs <- dplyr::select(covs, -strata)

new <- covs %>% 
  make_newdata(windspeed3 = seq_range(windspeed3, n = 100)) # holds all variables constant

new <- new %>%
  mutate(HR = predict(fit, ., type = "risk", na.action = na.pass))

ggplot(data = new) + 
  aes(x = windspeed3, y = HR) + 
  geom_line() + 
  xlab("Wind Speed (3-day mean)") + 
  ylab("Risk") + 
  theme_bw()

ggsave(filename = './figures/hr.png')

# Extract pland and dist_land3

covs <- attr(svf$strata, "names")
covs <-str_split(covs, ",")
covs <- data.frame(matrix(unlist(covs), nrow=length(covs), byrow=T))
colnames(covs) <- c("pland3", "dist_land3")
covs <- covs %>%
  mutate_all(~gsub("pland3=", "", .)) %>%
  mutate_all(~gsub("dist_land3=", "", .))

covs$pland3 <- as.numeric(covs$pland3)
covs$dist_land3 <- as.numeric(covs$dist_land3)

covs <- cbind(tidy_survfit, covs)
covs <- dplyr::select(covs, -strata)

new <- covs %>% 
  make_newdata(pland3 = seq_range(pland3, n = 100)) # holds all variables constant

new <- new %>%
  mutate(risk = predict(fit2, ., type = "risk", na.action = na.pass))

ggplot(data = new) + 
  aes(x = pland3, y = risk) + 
  geom_line() + 
  scale_x_reverse() + 
  xlab("Percent Ice Cover (3-day mean)") + 
  ylab("Risk") + 
  theme_bw()

sub <- subset(cox, migrate == 1)
