rm(list = ls())

load('all.RData')
library(dplyr)

# create columns for time spent on land and cumulative time on land
x = all %>% 
  group_by(id) %>% 
  arrange(id, datetime) %>% 
  mutate(time.land=ifelse(land==0 | is.na(lag(land)) | lag(land)==0, 
                          0,
                          difftime(datetime, lag(datetime), units="hours"))) %>% 
  mutate(cumtime.land=time.land + ifelse(is.na(lag(time.land)), 0, lag(time.land))) %>%
  mutate(cumtime.land2 = cumsum(time.land))

x.df <- as.data.frame(x) #convert to df

pb1 <- subset(x.df, animal=='pb_06336')
pb1 <- select(pb1, animal, datetime, land, time.land:cumtime.land2)











