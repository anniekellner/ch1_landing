rm(list = ls())
load('all.RData')
library(dplyr)

x = all %>% 
  group_by(id) %>% 
  arrange(id, datetime) %>% 
  mutate(time.land=ifelse(land==0 | is.na(lag(land)) | lag(land)==0, 
                          0,
                          difftime(datetime, lag(datetime), units="days"))) %>% 
  mutate(cumtime.land=time.land + ifelse(is.na(lag(time.land)), 0, lag(time.land)))

x.df <- as.data.frame(x)

pb1 <- subset(x.df, animal=='pb_06336')
pb1 <- select(pb1, animal, datetime, land, cumtime.land)











