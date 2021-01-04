######################################################
###   ADD BIOLOGICAL VARIABLES TO COX DATABASE  ######
######################################################

library(dplyr)


rm(list = ls())

load('Cox_TDC.RData')
bio <- read.csv('./data/raw-data/resid_mass_indfem_19832015.csv')

bio <- select(bio, BearID, capture.year, age, ResidualMass)

bio$animal <- paste0("pb_", bio$BearID)
bio$id <- paste(bio$animal, bio$capture.year, sep = ".")

bio <- bio %>%
  select(-c(BearID, capture.year))

cox_tdc <- left_join(cox_tdc, bio)

cox_tdc %>%
  group_by(id) %>%
  slice_head()

missing <- subset(bio, animal == "pb_20845") # no residmass but other measurements present - 20446
# 6817, 20333 (2008), 20418 (2005), 20520 (2012), 20529 (2005), 20735 (2009), 21264 (2011), 21358 (2013) 
