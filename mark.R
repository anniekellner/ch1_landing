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

ind <- dplyr::select(ind, -id) # dataframe must start with ch

#ind <- select(ind, id2, ch, group, repro)

#write.csv(ind, file = "test_run.csv") 

# ----------------------------------------------------------------------------------------------------- #

# Blackduck example

Blackduck$BirdAge=as.numeric(Blackduck$BirdAge)-1 # factor variables not allowed

# Process Data
#Prior to analyzing the data, this function initializes several variables (e.g., number of capture occasions,
#time intervals) that are often specific to the capture-recapture model being fitted to the data.
#It also is used to 1) define groups in the data that represent different levels of one or more factor
#covariates (e.g., sex), 2) define time intervals between capture occasions (if not 1), and 3) create an
#age structure for the data, if any.

bduck.processed = process.data(Blackduck, model = "Known")
ind.processed = process.data(ind, model = "Known") # looks good

# Create design dataframes for MARK model specification
#For each type of parameter in the analysis model (e.g, p, Phi, r), this function creates design data
#based on the parameter index matrix (PIM) in MARK terminology. Design data relate parameters to
#the sampling and data structures; whereas data relate to the object(animal) being sampled. Design
#data relate parameters to time, age, cohort and group structure.

bduck.ddl=make.design.data(bduck.processed)
ind.ddl <- make.design.data(ind.processed)

bduck.ddl$S$min=c(4,6,7,7,7,6,5,5) # no idea wtf this is

# Define range of models for S

# Blackduck
S.dot=list(formula=~1) # standard model with no variability # estimating 1 param
S.time=list(formula=~time) # different S for each occasion (year?) - now estimating 8 params
S.min=list(formula=~min)
S.BirdAge=list(formula=~BirdAge)
S.BirdAgexWeight.min=list(formula=~min+BirdAge*Weight)
S.BirdAge.Weight=list(formula=~BirdAge+Weight)



# Create model list and run assortment of models

#Creates a dataframe of all combinations of parameter specifications for each parameter in a particular
#type of MARK model. It is used together with mark.wrapper to run a series of models from
#sets of parameter specifications.

model.list=create.model.list("Known")

# Constructs and runs a set of MARK models from a dataframe of parameter specifications
results=mark.wrapper(model.list,data=ind.processed,ddl=ind.ddl, run = FALSE)
bduck.results

mydata <- ind
mydata$repro <- as.numeric(mydata$repro)

# Process data
mydata.processed <- process.data(mydata, model = "Known") # initializes variables, defines groups
mydata.ddl <- make.design.data(mydata.processed)

# PB example models

S.dot=list(formula=~1)
S.Repro = list(formula=~repro)
S.time=list(formula=~time)


model.list=create.model.list("Known")

mydata.results=mark.wrapper(model.list,data=mydata.processed,ddl=mydata.ddl)

mydata.results
