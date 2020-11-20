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
start$ordinal <- yday(start$datetime)

table(start$year)

first <- min(start$ordinal)
last <- max(start$ordinal)

# Assign random start dates to all bears

random <- sample(199:264, 22, replace = TRUE) 

start$random <- random

data <- dplyr::select(start, year, ordinal, random)

diff <- data %>%
  group_by(year) %>%
  
  
  