rm(list = ls())

load('all.RData')

all$ice <- ifelse(all$land==1,0,1) #create 'ice' column (will include some swimming)

library(dplyr)
library(lubridate)

all$ymd <- ymd(paste(all$year, all$month, all$day)) 

# count how many ice points each day w/ reset by id and day
w.day = all %>%
  group_by(id, ymd) %>%
  arrange(id, datetime) %>%
  mutate(number.ice = cumsum(ice)) 

# create df of last rows to identify when bear has not been on ice all day (1)
flag = w.day %>%
  group_by(id, ymd) %>%
  arrange(id, datetime) %>%
  slice(n()) %>%
  mutate(flag = ifelse(number.ice==0,0,1))

# merge flag df with w.day df
newdef.ice <- left_join(w.day, flag)
newdef.ice[is.na(newdef.ice)] <- 1 #NA will show up for all values not in 'flag' dataset. These are not important so -->1

# create columns for time spent on ice and cumulative time on ice
ice = newdef.ice %>% 
  group_by(id) %>% 
  arrange(id, datetime) %>% 
  mutate(time.ice=ifelse(ice==0 | is.na(lag(ice)) | lag(ice)==0 | flag==0, 
                          0,
                          difftime(datetime, lag(datetime), units="hours"))) 

ice.df <- as.data.frame(ice)

# create column with cumulative time on land (in days), reset when flag = 0
library(data.table)
setDT(ice.df)
ice.df[, cum.ice := flag*cumsum(time.ice), .(id, rleid(flag))]

ice.df$cum.ice = ice.df$cum.ice/24

save(ice.df, file='NewDef.ice.RData')

###### Split dataset into June through November and slice by last cum.ice >7

load('NewDef.ice.RData')
library(dplyr)

jun.nov <- subset(ice.df, month >5 & month < 11) # excluded Dec because bears go back out onto ice
ice.ded <- subset(jun.nov, cum.ice >=7 & ice==1)

last <- ice.ded %>%
  group_by(id) %>%
  arrange(id,datetime) %>%
  slice(n())

write.csv(first, file='C:/Users/akell/Desktop/Spring 2019/Research/last_ice_7_days.csv')


