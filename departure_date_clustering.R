####################################################################################  
##    ARE INTRANNUAL DEPARTURE DATES CLOSER THAN WOULD BE EXPECTED AT RANDOM?   #####
###################################################################################

# A coarse analysis that would be worth adding is the clustering of departure dates within a year. 
# You could assess if timing is distributed randomly or more clustered than expected in random. 
# An approach to do this would be to look at average number of days separate randomly assigned departure dates 
# within a year (so assign same number of bears to random days between the earliest and latest julian departure dates) 
# compared to the average difference of observed bears within a year.


# 12 original datapoints

rm(list = ls())

library(lubridate)
library(dplyr)

load("logreg.RData")

start <- subset(logreg, start.swim == 1)
start$ordinal <- yday(start$datetime)

table(start$year)

first <- min(start$ordinal)
last <- max(start$ordinal)

sample <- matrix(nrow = 4, ncol)

year1 <- sample(199:264, 2, replace = TRUE) 
year2 <- sample(199:264, 2, replace = TRUE)
year3 <- sample(199:264, 3, replace = TRUE)
year4 <- sample(199:264, 2, replace = TRUE)

as.numeric(dist(year3))
sample_diffs <- c(diff(year1), diff(year2), dist(year3), diff(year4))

start <- subset(start, year == "2009" | year == "2011" | year == "2013" | year == "2014")

test <- start %>%
  