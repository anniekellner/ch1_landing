########################################
###   BASIC DESCRIPTIVE STATS   ########
########################################

rm(list= ls())

library(lubridate)

land <- readRDS('./data/RData/land_bears_CoxPH.Rds')
cox <- readRDS('./data/RData/cox_tdc.Rds')

sub <- subset(land, start.swim == 1)

# Departure Date

sub$ordinal <- yday(sub$ymd)
mean(sub$ordinal)
min(sub$ordinal)
max(sub$ordinal)

# Distance to shore

sub2 <- subset(cox, migrate == 1)
mean(sub2$dist_land)
sd(sub2$dist_land)
min(sub2$dist_land)
max(sub2$dist_land)

