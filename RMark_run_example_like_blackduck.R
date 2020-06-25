########################################################################################
############    LEARN RMARK   ##########################################################
########################################################################################

# Copied Blackduck script from RMark vignette and was able to run bears with repro data only 


rm(list = ls())

library(dplyr)
library(RMark)

load('all_v2.RData')
load('KFM.RData')
data("Blackduck")

# Add reproductive status 
# 0 = none/unknown
# 1 = denning
# 2 = coy
# 3 = yearling 
# NOTE: combine coy and yearling and see if effect is different

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
bears <- dplyr::select(bears, -id) 
bears <- as.data.frame(bears) # not sure if RMark will accept tibble format


# Change BirdAge to numeric; starting with version 1.6.3 factor variables are
# no longer allowed. They can work as in this example but they can be misleading
# and fail if the levels are non-numeric. The real parameters will remain
# unchanged but the betas will be different.

run.bears=function()
{
  #
  # Process data
  #
  bears.processed=process.data(bears,model="Known")
  #
  # Create default design data
  #
  bears.ddl=make.design.data(bears.processed)
  #

  
  # Define range of models for S
  #
  S.dot=list(formula=~1) # survival is equal for both repro states and over time
  S.repro=list(formula=~repro)
  #
  # Create model list and run assortment of models
  #
  model.list=create.model.list("Known")
  bears.results=mark.wrapper(model.list,data=bears.processed,ddl=bears.ddl,
                             invisible=FALSE,threads=1)
  #
  # Return model table and list of models
  #
  return(bears.results)
}
bears.results=run.bears()
bears.results
