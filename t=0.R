#########################################################
###   DETERMINING TIME OF ORIGIN FOR TTE MODEL ##########
#########################################################

# Calling t= 0 on average date when SIC is ~ 50% over the shelf and falls shortly thereafter.
# 50% is considered the lower threshold for suitable habitat for polar bears (T Atwood and D. Douglas pers. comm.)
# icemean_50 is a mean SIC of 50% over the entire shelf

library(dplyr)
library(lubridate)
library(tidyselect)


rm(list = ls())

# Format data

ice <- read.csv("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Data/sbs_daily_icestats_1979_2017_v6.csv")

class(ice$date) # Date imported as factor 1/1/1979

ice$date <- mdy(ice$date, tz = NULL)
ice$month <- month(ice$date)
ice$year <- year(ice$date)

ice <- ice %>%
  mutate(ordinal = yday(date)) %>%
  select(date, ordinal, year, month, starts_with("sbs_shelf") & ends_with("_50")) %>%
  filter(month < 9)

icepct <- ice %>%
  group_by(year(date)) %>%
  filter(sbs_shelf_icepct_50 > 50) %>%
  slice(n())
  
icemean <- ice %>%
  group_by(year(date)) %>%
  filter(sbs_shelf_icemean_50 > 85) %>%
  slice(n()) 

# June 1 is when 85% of the shelf is covered by 50% mean ice concentration


