######################################
##            WIND             #######
######################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)


setwd('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Data')
# load and format data

wind <- read.csv('movebank_v5_annotate.csv')
wind <- select(wind, 1:5,8,15)
colnames(wind) <- c('ID', 'Date', 'Time','long', 'lat', 'U', 'V') 

# Reformat date 

wind$datetime <- paste(wind$Date, wind$Time) 
wind$datetime <- mdy_hms(wind$datetime)


#wind$Date <- as.POSIXct(wind$Date)
#wind$year <- year(wind$Date)

# Index rows by date

wind2 <- wind %>%
  group_by(ID) %>%
  arrange(ID, datetime) %>%
  mutate(mph=sqrt((U*U) + (V*V))) %>%
  mutate(index=difftime(last(datetime), datetime, units='days'))

wind2$depart <- ifelse(wind2$index==0, 1,0) # flag departures

wind2$index <- as.numeric(wind2$index)


# plot one animal to see what it looks like
ggplot(wind2[wind2$ID =='pb_20414.2009',], aes(index, mph)) +
  geom_line(aes(color = 'red')) +
  scale_x_reverse()


# ------------------------------------------------------------------------------------------------------- #

# Double-check timezone against raw data

load('C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Repos/ch1_landing/all_v2.RData')
head(ice)
head(wind2)
wind2 <- as.data.frame(wind2)


# Plot wind speed vs. time

ggplot(wind2, aes(index, mph, color=ID, na.rm=TRUE)) +
  stat_summary(geom = 'line', fun.y = 'mean') +
  xlab("Day Before Departure") +
  ylab('mph') +
  ggtitle('Wind Speed 30 Days Before Departure') +
  #facet_wrap(~year) +
  theme_bw()

# ----  Test WindDir Function ------------------------------------------ #
 
dd <- read.csv("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Analyses/uvWind_2_speedDirection_v01.csv")

test <- WindDir(dd$u, dd$v)

dd <- cbind(dd, test) 
