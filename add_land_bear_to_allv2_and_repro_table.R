##################################################################################
##      ADDS "LAND BEAR" STATUS TO ALL.V2 DATABASE    ############################
##        ############################
##################################################################################

# CREATES TABLE FOR REPRODUCTIVE STATUS 

rm(list = ls())

library(dplyr)

load('all_v2.RData')
load('land_bears_ows.RData')


ows$repro <- 0 # 242 bears in ows


ows$land_bear <- ifelse(ows$id %in% lb$id, 1, 0)
test <- subset(ows, land_bear == 1); unique(test$id) # 23 land bears


repro <- ows %>% # 242 bears in repro
  mutate(repro = replace(repro, DenYr == 1, 1)) %>%
  mutate(repro = replace(repro, coy == 1, 2)) %>%
  mutate(repro = replace(repro, yearling == 1, 3)) %>%
  dplyr::select(id, land_bear, repro)

repro$repro <- factor(repro$repro, labels = c("Unknown", "Denning", "COY", "Yearling"))


repro1 <- repro %>%
  group_by(id) %>%
  slice_head()

sub.land.repro1 <- subset(repro1, land_bear == 1) # 23 land bears

table(repro1$land_bear_ows, repro1$repro) # Not correct numbers

repro1 <- repro1 %>%
  mutate(bearType = if_else(land_bear == 1, "land", "ice"))

table(repro1$bearType, repro1$repro)

#save(all.v2, file = "all_v2.RData")

####

