##############################################################
##    TRY MULTIPLE IMPUTATION   ##############################
##############################################################

# Can run once and get 3 values to replace NA's in ResidMass
# Probably supposed to run more than once (default m = 5)

# Values from m = 1
# -41.6, 51.1, -26.9

library(dplyr)
library(mice)

mydata <- readRDS('./data/RData/ph_Dec7.Rds')

head(mydata)

mydata <- mydata %>%
  group_by(id) %>%
  slice_head()

bc <- select(mydata, ResidMass)

# Remove mean values
bc$ResidMass <- ifelse(bc$ResidMass < -8 & bc$ResidMass > -9, NA, bc$ResidMass)

# Impute 3 new values using mice package

imp <- mice(bc, m=1)
imp

# Add new values to dataframe


