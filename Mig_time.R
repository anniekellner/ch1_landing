rm(list = ls())
library(dplyr)

load('all.RData')
start <- subset(all, start.swim==1)
start <- droplevels(start)
end <- subset(all, end.swim==1)
end <- droplevels(end)

mig <- left_join(start,end, by='id')
mig <- select(mig, animal.x, datetime.x, datetime.y)

colnames(mig) <- c('animal', 'start', 'end')

mig$diff <- difftime(mig$end, mig$start, tz='US/Alaska', units="days")


