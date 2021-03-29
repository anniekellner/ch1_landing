########################################
##    DESCRIPTIVE STATS   ##############
########################################

rm(list = ls())

library(dplyr)

d <- readRDS('./data/derived-data/avg.Rds')

mig <- filter(d, start_swim == 1)

# Distance metrics

mean(mig$dist_pack)
min(mig$dist_pack)
max(mig$dist_pack)
sd(mig$dist_pack)

mean(mig$dist_land)
max(mig$dist_land)
min(mig$dist_land)
sd(mig$dist_land)

mean(mig$ordinal_day)
min(mig$ordinal_day)
max(mig$ordinal_day)
sd(mig$ordinal_day)

# 