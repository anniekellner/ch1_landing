rm(list = ls())

library(dplyr)

load('all_v2.RData')
load('KFM.RData')

all.v2$repro <- 0

all.v2 <- all.v2 %>%
  mutate(repro = replace(repro, DenYr == 1, 1)) %>%
  mutate(repro = replace(repro, coy == 1, 2)) %>%
  mutate(repro = replace(repro, yearling == 1, 3))

#test <- subset(all.v2, coy ==1)

repro <- all.v2 %>%
  select(id, repro) %>%
  distinct() 

ch.ids <- unique(ch.all$id)
repro <- subset(repro, id %in% ch.ids)

bears <- left_join(ch.all, repro)
bears <- ungroup(bears)

save(bears, file = 'KFM.RData') # save reproduction as individual covariate