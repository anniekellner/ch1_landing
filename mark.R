######################################################################################
#####     ADD INDIVIDUAL AND ENVIRONMENTAL VARIABLES TO MARK-FORMATTED DATA   ########
######################################################################################

rm(list = ls())

library(dplyr)
library(RMark)


# Add reproductive status 
# 0 = none/unknown
# 1 = denning
# 2 = coy
# 3 = yearling 
# NOTE: combine coy and yearling and see if effect is different

load('all_v2.RData')
load('KFM.RData')
data("Blackduck")

# change reproductive status to categorical variable in all.v2

all.v2$repro <- 0

all.v2 <- all.v2 %>%
  mutate(repro = replace(repro, DenYr == 1, 1)) %>%
  mutate(repro = replace(repro, coy == 1, 2)) %>%
  mutate(repro = replace(repro, yearling == 1, 3))

#test <- subset(all.v2, coy ==1)

repro <- all.v2 %>%
  select(id, repro) %>%
  distinct() 

  
# remove id's that are not in ch dataset
ch.ids <- unique(ch.all$id)
repro <- subset(repro, id %in% ch.ids)

ind <- left_join(ch.all, repro)

# ---------------------------------------------------------------------------------------------------- #

# Formatting for trial run via A. Dillon

#ind$group <- 1 

#ind$id2 <- paste0("/*", ind$id, "*/")
ind <- ungroup(ind)

ind <- dplyr::select(ind, -id) # dataframe must start with c

#ind <- select(ind, id2, ch, group, repro)

#write.csv(ind, file = "test_run.csv") 

# ----------------------------------------------------------------------------------------------------- #

# Blackduck example

Blackduck$BirdAge=as.numeric(Blackduck$BirdAge)-1
bduck.processed = process.data(Blackduck, model = "Known")
bduck.ddl=make.design.data(bduck.processed)
bduck.ddl$S$min=c(4,6,7,7,7,6,5,5)

S.dot=list(formula=~1) # standard model with no variability # estimating 1 param
S.time=list(formula=~time) # different S for each occasion (year?) - now estimating 8 params
S.min=list(formula=~min)
S.BirdAge=list(formula=~BirdAge)

S.BirdAgexWeight.min=list(formula=~min+BirdAge*Weight)
S.BirdAge.Weight=list(formula=~BirdAge+Weight)
#
# Create model list and run assortment of models



model.list=create.model.list("Known")
bduck.results=mark.wrapper(model.list,data=bduck.processed,ddl=bduck.ddl, run = FALSE,
                           invisible=FALSE,threads=1)
bduck.results

mydata <- ind

# Process data
mydata.processed <- process.data(mydata, model = "Known") # initializes variables, defines groups
mydata.ddl <- make.design.data(mydata.processed)

S.dot=list(formula=~1)
S.Repro = list(formula=~repro)

model.list=create.model.list("Known")

mydata.results=mark.wrapper(model.list,data=mydata.processed,ddl=mydata.ddl)

mydata.results
