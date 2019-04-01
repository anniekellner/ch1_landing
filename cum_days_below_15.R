###################################################
### CUMULATIVE TIME AT < 15% SIC ##################
###################################################

# Calculate for each bear the number of days <15%
# Calculate number of days as percentage of all days
# Plot distribution 
  # x-axis: time since at 15%
  # y-axis: cumulative % time <= 15%

#-------------------------------------------------#

rm(list = ls())
load('all_v2.RData')
library(dplyr)
library(data.table)
library(ggplot2)

SIC <- subset(all.v2, SIC >=0, na.rm=TRUE)
SIC <- SIC %>%
  filter(!(id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004')) #remove undecided

# Flag columns with <15% SIC

flag.15 = SIC %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(flag.15 = ifelse(SIC<15,1,0))

# add time

x= flag.15 %>%
  group_by(id) %>%
  arrange(id,datetime) %>%
  mutate(time.15 = ifelse(flag.15==0 | lag(flag.15)==0,
         0, difftime(datetime, lag(datetime), units='days'))) %>%
  mutate(cumtime.15 = cumsum(ifelse(is.na(time.15), 0, time.15)) + time.15*0) %>%
  mutate(pct.days.below15 = cumtime.15/30) %>%
  mutate(index=difftime(last(ymd), ymd, units='days')) # so day 1 is day 1 for all bears, regardless of month/year

ice.calc <- as.data.frame(x)
save(ice.calc, file='ice_calc.RData')
#-------------- GGPLOT ---------------------------------#

# Number of days Spent in 15% SIC or less - all bears

ggplot(ice.calc, aes(index, cumtime.15, color=id, na.rm=TRUE)) +
  stat_summary(geom = 'line', fun.y = 'mean') +
  scale_x_reverse() +
  xlab("Days before Departure") +
  ylab('Cumulative # of days spent at < 15% SIC') +
  ggtitle('Days before departure spent at < 15% SIC') +
  theme_bw()

#saved C:\Users\akell\Desktop\Spring 2019\Presentations\Alaska\days_15pct.pdf

#------------- MEAN AND STDEV------------------------------------#

last <- ice.calc %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice(n())

mean (last$cumtime.15) #20.13 days
sd(last$cumtime.15) #8.77 days 
## TO DO: Make sure each animal only has 30 days (not 'one month' of data)

