rm(list = ls())
library(dplyr)

load('ows_land.RData')

# bone pile = 1, non-bonepile=0
ows.land$bonepile <- ifelse(ows.land$animal=='pb_20446' | ows.land$animal=='pb_20529' | ows.land$animal=='pb_21237',0,1)

save(ows.land, file='ows_land.RData') #update ows_land.RData with bonepile distinction 


