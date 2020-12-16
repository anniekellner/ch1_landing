load('all_v2.RData')
head(all.v2)

library(dplyr)

vj <- subset(all.v2, land_bear == 1)

vj <- vj %>%
  group_by(id) %>%
  filter(land == 1) %>%
  slice_head()

load('land_bears_CoxPH.RData')
head(bears)

start <- subset(bears, start.swim == 1 | end.swim == 1)

start <- start %>% distinct()

write.csv(start, 'for_vijay.csv')
