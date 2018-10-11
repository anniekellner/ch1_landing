rm(list = ls())

load('Migration_dist.RData')
load('Str_line_dist.RData')

start.end.LL$Distance <- as.numeric(start.end.LL$Distance) 
start.end.LL$Distance <- start.end.LL$Distance/1000

colnames(swim.len) <- c('id', 'bear_dist')

library(dplyr)
names(start.end.LL)[6] <- 'crow_dist'
swim.len <- left_join(swim.len, start.end.LL, by='id')

swim.len <- mutate(swim.len, ratio=bear_dist/crow_dist)

save(swim.len, file='Migration_dist.RData')
