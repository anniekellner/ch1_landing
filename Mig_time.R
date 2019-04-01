rm(list = ls())
library(dplyr)

load('all_v2.RData')

start <- subset(all, start.swim==1)
start <- start %>% # remove 'undecided'
  filter(!(id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004'))
start <- droplevels(start)

end <- subset(all, end.swim==1)
end <- end %>% # remove 'undecided'
  filter(!(id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004'))
end <- droplevels(end)

mig <- left_join(start,end, by='id')
mig <- select(mig, animal.x, datetime.x, datetime.y)

colnames(mig) <- c('animal', 'start', 'end')

mig$diff <- difftime(mig$end, mig$start, tz='US/Alaska', units="days")


