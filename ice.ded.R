rm(list = ls())

load('all.RData')

all$ice <- ifelse(all$land==1,0,1) #create 'ice' column (will include some swimming)

library(dplyr)
library(lubridate)

all$ymd <- ymd(paste(all$year, all$month, all$day)) 

# count how many land points each day w/ reset by id and day
w.day = all %>%
  group_by(id, ymd) %>%
  arrange(id, datetime) %>%
  mutate(number.ice = cumsum(ice)) 

# create df of last rows to identify when bear has not been on land all day (1)
flag = w.day %>%
  group_by(id, ymd) %>%
  arrange(id, datetime) %>%
  slice(n()) %>%
  mutate(flag = ifelse(number.ice==0,0,1))

# merge flag df with w.day df
newdef.ice <- left_join(w.day, flag)
newdef.ice[is.na(newdef.ice)] <- 1