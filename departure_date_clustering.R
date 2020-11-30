####################################################################################  
##    ARE INTRANNUAL DEPARTURE DATES CLOSER THAN WOULD BE EXPECTED AT RANDOM?   #####
###################################################################################

# A coarse analysis that would be worth adding is the clustering of departure dates within a year. 
# You could assess if timing is distributed randomly or more clustered than expected in random. 
# An approach to do this would be to look at average number of days separate randomly assigned departure dates 
# within a year (so assign same number of bears to random days between the earliest and latest julian departure dates) 
# compared to the average difference of observed bears within a year.


# 22 original datapoints

rm(list = ls())

library(lubridate)
library(dplyr)

load("land_bears_CoxPH.RData")

start <- subset(bears, start.swim == 1)
start <- distinct(start)

start$ordinal <- yday(start$datetime)

table(start$year)

first <- min(start$ordinal)
last <- max(start$ordinal)

start$year <- as.numeric(start$year)

# create dataframe with range of departure dates per year

data <- start %>%
  group_by(year) %>%
  filter(n() > 1) %>% # exclude years with only one departure
  dplyr::select(id, year, ordinal) %>%
  arrange(year, ordinal) %>%
  filter(row_number() == 1 | row_number() == n()) %>% # select two numbers at extreme ends of range
  mutate(diff = ordinal - min(ordinal)) %>% # calculate difference in number of days
  filter(diff != 0)

values <- data.frame("year" = c(2005, 2006, 2009, 2011, 2013, 2014))
values$Pct_fewer <- 0
  
# 2005

set.seed(13) 
r1 <- sapply(1:10000, function(i) diff(range(sample(first:last, 2, replace = T))))
yr2005 <- mean(r1 > 32)
values[1,2] <- yr2005
head(values)

# 2006

set.seed(13)
r2 <- sapply(1:10000, function(i) diff(range(sample(first:last, 2, replace = T))))
yr2006 <- mean(r2 > 7)
values[2,2] <- yr2006

# 2009

set.seed(13)
r3 <- sapply(1:10000, function(i) diff(range(sample(first:last, 3, replace = T))))
yr2009 <- mean(r3 > 10)
values[3,2] <- yr2009

#2011

set.seed(13)
r4 <- sapply(1:10000, function(i) diff(range(sample(first:last, 2, replace = T))))
yr2011 <- mean(r4 > 9)
values[4,2] <- yr2011

# 2013

set.seed(13)
r5 <- sapply(1:10000, function(i) diff(range(sample(first:last, 3, replace = T))))
yr2013 <- mean(r5 > 3)
values[5,2] <- yr2013

# 2014

set.seed(13)
r6 <- sapply(1:10000, function(i) diff(range(sample(first:last, 4, replace = T))))
yr2014 <- mean(r6 > 3)
values[6,2] <- yr2014

  