######################################
##            WIND             #######
######################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)

# load and format data

wind <- read.csv('./Data/movebank_v5_annotate.csv')
wind <- select(wind, 1:5,8,15)
colnames(wind) <- c('ID', 'Date', 'Time','long', 'lat', 'U', 'V') 

# Reformat date 

wind$datetime <- paste(wind$Date, wind$Time) 
wind$datetime <- as.POSIXct(wind$Date, tz='UTC')
wind$ymd <- ymd(wind$Date) 
wind$Date <- as.POSIXct(wind$Date)
wind$year <- year(wind$Date)

# Index rows by date

wind2 <- wind %>%
  group_by(ID) %>%
  arrange(ID, datetime) %>%
  mutate(mph=sqrt((U*U) + (V*V))) %>%
  mutate(index=difftime(last(ymd), ymd, units='days'))

# Plot wind speed vs. time

ggplot(wind2, aes(index, mph, color=ID, na.rm=TRUE)) +
  stat_summary(geom = 'line', fun.y = 'mean') +
  xlab("Day Before Departure") +
  ylab('mph') +
  ggtitle('Wind Speed 30 Days Before Departure') +
  facet_wrap(~year) +
  theme_bw()
  