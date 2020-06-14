######################################################################################
#####     ADD INDIVIDUAL AND ENVIRONMENTAL VARIABLES TO MARK-FORMATTED DATA   ########
######################################################################################

rm(list = ls())

library(dplyr)


# Add reproductive status 
# 0 = none/unknown
# 1 = denning
# 2 = coy
# 3 = yearling 
# NOTE: combine coy and yearling and see if effect is different

load('all_v2.RData')
load('KFM.RData')

# change reproductive status to categorical variable in all.v2

all.v2$repro <- 0

all.v2 <- all.v2 %>%
  mutate(repro = replace(repro, DenYr == 1, 1)) %>%
  mutate(repro = replace(repro, coy == 1, 2)) %>%
  mutate(repro = replace(repro, yearling == 1, 3))

repro <- all.v2 %>%
  select(id, repro) %>%
  distinct()
  
# remove id's that are not in ch dataset
ch.ids <- unique(ch.all$id)
repro <- subset(repro, id %in% ch.ids)

ind <- left_join(ch.all, repro)





