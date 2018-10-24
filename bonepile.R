####################
## Bonepile Bears ##
####################

rm(list = ls())

load('all.RData')
load('ows_land.RData')

library(dplyr)
library(lubridate)
library(ggplot2)

# as.Date 
start <- subset(all, start.swim==1) %>%  droplevels() 
start$ymd <- paste(start$year, start$month, start$day, sep = '-')
start$ymd <- ymd(start$ymd)

#convert to doy
start$doy <- strftime(start$ymd, format="%j")
start$doy <- as.numeric(start$doy)

start$md <- format(strptime(start$doy, format="%j"), format="%b-%d") 

#Subset bonepile = 0
nb <- subset(ows.land, bonepile==0) %>% droplevels()


# plot density curve
ggplot(start, aes(doy))+
  geom_density() +
  xlab("Day-of-Year") +
  geom_vline(xintercept = 212, color="blue") +
  geom_vline(xintercept = 222, color="blue") +
  geom_vline(xintercept = 258, color="blue") +
  ggtitle("Distribution of departure dates: from ice to land") 
   
 




