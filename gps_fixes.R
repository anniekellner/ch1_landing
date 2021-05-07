################################################
###   CALCULATE GPS VARIABILITY   ##############
################################################

rm(list = ls())

library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)


source('MyFunctions.R')

load('all_v2.RData')
gps <- readRDS('./data/RData/land_bears_CoxPH.Rds') # only bears in study

land <- gps
land <- subset(lb, land_bear_ows == 1)

land <- select(land, animal, year, month, id, datetime)

land <- droplevels(land)

land <- land %>%
  group_by(id) %>%
  filter(month > 5 & month < 10) %>%
  mutate(diff = datetime - lag(datetime),
         diff_hrs = as.numeric(diff, units = 'hours')) %>%
  mutate(skipped = (diff_hrs / lag(diff_hrs) - 1))

land$skipped <- ifelse(land$skipped < 0, 0, land$skipped) # change negative skipped values to 0

land$skipped <- round(land$skipped, digits = 0)

# Remove all rows after swim initiated

land <- land %>%
  group_by(id) %>%
  mutate(day = row_number()) 

land <- land %>% 
  group_by(id) %>%
  mutate(across(everything(),
                ~replace(., row_number() > match(1, start.swim), NA))) %>%
  ungroup()


medians <- land %>% # use median to find fix intervals
  group_by(id) %>%
  summarise(fixes = median(diff_hrs, na.rm = TRUE))

total <- land %>% # total observations
  group_by(id) %>%
  tally()

missed <- land %>%
  group_by(id) %>%
  summarise(skipped = sum(skipped, na.rm = TRUE)) %>%
  left_join(total) 

ratio <- missed %>%
  mutate(proportion_skipped = skipped/(n + skipped)) %>%
  drop_na()

mean(ratio$proportion_skipped)
min(ratio$proportion_skipped)
sd(ratio$proportion_skipped)


ggplot(data = land, aes(diff_hrs)) + 
  geom_histogram(binwidth = 1)

