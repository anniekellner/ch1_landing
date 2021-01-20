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

#load('all_v2.RData')
cox <- readRDS('./data/RData/cox_tdc.Rds')

library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)
library(tidyr)

# Flag columns with <15% SIC

flag = cox %>%
  group_by(id) %>%
  arrange(id, tstart) %>%
  mutate(flag = ifelse(SIC <15 ,1,0))

flag = flag %>%
  group_by(id) %>%
  mutate(days_under30 = cumsum(flag))

sum <- flag %>%
  group_by(id) %>%
  summarise(days = sum(flag)) 

sum <- sum %>% 
  separate(id, c("pb", "animal", "year"))

# add time

#x= flag.15 %>%
  #group_by(id) %>%
  #arrange(id,datetime) %>%
  #mutate(time.15 = ifelse(flag.15==0 | lag(flag.15)==0,
         #0, difftime(datetime, lag(datetime), units='days'))) %>%
  #mutate(cumtime.15 = cumsum(ifelse(is.na(time.15), 0, time.15)) + time.15*0) %>%
  #mutate(pct.days.below15 = cumtime.15/30) %>%
 # mutate(index=difftime(last(ymd), ymd, units='days')) # so day 1 is day 1 for all bears, regardless of month/year

#ice.calc <- as.data.frame(x)
#save(ice.calc, file='ice_calc.RData')
#-------------- GGPLOT ---------------------------------#

# Number of days Spent in 15% SIC or less - all bears

ggplot(flag, aes(tstart, days_under30, color=id, na.rm=TRUE)) + # needs color line
  geom_line() + 
  xlab("Days before Departure") +
  ylab('Cumulative # of days spent at < 30% ice cover') +
  ggtitle('Days before departure spent at < 30% ice cover') +
  theme_bw()

# Histogram

ggplot(data = sum, aes(days, fill = year)) +
  geom_histogram(alpha = 0.2, bins = 6) + 
  geom_density(alpha = 0.2, fill = "#FF6666")

ggplot(data = sum, aes(days)) +
  geom_histogram(aes(y = ..density..), binwidth = 4, color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#FF6666") +
  scale_x_continuous(limits = c(0,70), expand = c(0,0)) + 
  xlab("Days spent at < 15% sea ice concentration")

ggsave(filename = 'Days_below_15.png', path = './figures')


#saved C:\Users\akell\Desktop\Spring 2019\Presentations\Alaska\days_15pct.pdf

#------------- MEAN AND STDEV------------------------------------#

last <- ice.calc %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice(n())

mean(sum$days, na.rm = TRUE) # 22.72 days
sd(sum$days, na.rm = TRUE) # 13.15 days 


