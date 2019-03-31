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

SIC <- subset(all.v2, SIC >=0, na.rm=TRUE)

# Flag columns with <15% SIC

flag = SIC %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(flag = ifelse(SIC<15,1,0))

# add time

x= flag %>%
  group_by(id) %>%
  arrange(id,datetime) %>%
  mutate(time.15 = ifelse(flag==0 | lag(flag)==0,
         0, difftime(datetime, lag(datetime), units='days'))) %>%
  mutate(cumtime.15 = cumsum(ifelse(is.na(time.15), 0, time.15)) + time.15*0)

x <- as.data.frame(x)

## TO DO: calculate cumtime.15 as percentage of all days and plot - include this in dplyr grouping so can do length of rows in id grouping

