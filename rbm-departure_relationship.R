#################################################################################
##    Figure out relationship between weight measurement (for RBM) and departure date #####
#################################################################################

rm(list = ls())

library(dplyr)
library(sf)
library(lubridate)
library(tidyr)

source('MyFunctions.R')

ph <- readRDS('./data/RData/land_bears_cutoff_after_swim.Rds')
mig <- filter(ph, start.swim == 1)
mig <- st_drop_geometry(mig)

mig <- mig %>%
  select(id, ymd) %>%
  rename(depart.date = ymd)

wt <- read.csv('./data/raw-data/resid_mass_indfem_19832015.csv')

study <- mig$id

wt$BearID <- paste('pb', wt$BearID, sep = "_")
wt$BearID <- paste(wt$BearID, wt$capture.year, sep= '.')

weights <- subset(wt, wt$BearID %in% study)

combine <- weights %>%
  rename(id = BearID) %>%
  separate(DateTime, c("Date", "Time"), sep = " ") %>%
  select(id, 1:2) %>%
  left_join(mig)

combine$Date <- mdy(combine$Date) 

# Calculate time difference between weighing and departure

combine  <- combine %>%
  mutate(time_diff = difftime(depart.date, Date, unit = "days"))
