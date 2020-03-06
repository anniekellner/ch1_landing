################################################
##      DISTANCE STATS    ######################
################################################

library(dplyr)

# Mean number of days away from ice pack

# load data

load("ice_calc.RData")
tt <- NULL

mydata <- select(ice, animal:gps_lon, id, datetime, ymd, dist2pack)

# Flag columns in which dist > 0

flag = mydata %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(flag = ifelse(dist2pack>0,1,0))

# add time 

x= flag %>%
  group_by(id) %>%
  arrange(id,datetime) %>%
  mutate(time.off.pack = ifelse(flag==0 | lag(flag)==0,
                          0, difftime(datetime, lag(datetime), units='days'))) %>%
  mutate(cumtime.offpack = cumsum(ifelse(is.na(time.off.pack), 0, time.off.pack)) + time.off.pack*0) %>%
  mutate(pct.days.offpack = cumtime.offpack/30) %>%
  mutate(index=difftime(last(ymd), ymd, units='days')) 

as.data.frame(x)

# select last entry for each bear

last <- x %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice(n())

as.data.frame(last)

# -------------------------------------------------------------------------------------------------- #

# ANALYSES

mean (last$cumtime.offpack) # 20.99 days
sd(last$cumtime.offpack) # 10.42 days 

mean(last$dist2pack) # 38908.6
sd(last$dist2pack) # 70435

## What is the best way to present this data? 
