################################################
#######   FORMAT DATA FOR RMARK   ########################
################################################

rm(list = ls())

library(RMark)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(tmap)
library(sf)

load('land_bears_ows.RData')


data("Blackduck") # example from RMark
head(Blackduck)

# remove undecided bears

lb <- filter(lb, id != "pb_20413.2006")
lb <- filter(lb, id != "pb_20418.2005")
lb <- filter(lb, id != "pb_20520.2012")
lb <- filter(lb, id != "pb_20529.2004")
lb <- filter(lb, id != "pb_20333.2008")
lb <- filter(lb, id != "pb_21307.2012")
lb <- filter(lb, id != "pb_21307.2014")
lb <- filter(lb, id != "pb_20446.2009")

# visual inspection

tmap_mode("view")

tm_shape(lb) +
  tm_dots(col = "month", size = 1, popup.vars = c("month", "day")) +
  tm_facets(by = "id") +
  tmap_options(limits = c(facets.view = 16))


data("Blackduck") # example from RMark
head(Blackduck)


# --  FUNCTIONS  ---------------------------------------------------------------------------- #

prep_data <- function(df) {
    df <- as_tibble(df)
    df$on.ice <- ifelse(df$swim == 1 | df$land == 1, 0, 1)
    df$ordinal <- yday(df$ymd)
    df <- select(df, id, ymd, ordinal, on.ice, land, start.swim)
  return(df)
}

ch <- prep_data(lb)

ss <- subset(lb, start.swim == 1) # 12 bears with start swim date

# ---   Format capture history (ch) for known-fate models ----------------------- #


# Create capture histories
# https://jamesepaterson.github.io/jamespatersonblog/07_creatingcapturehistories
# Chapter 16, Cooch & White 2020

 
ch$eh1 <- 1 # all recorded observations = 1 (first number in paired ch)

ch <- ch %>% # if there were both on and off land observations in a single day, took first
  group_by(id, ordinal) %>%
  slice(1) %>%
  distinct() # remove duplicates

# No start.swim entries were eliminated when first daily observation taken


# Remove bears with < 7 days on land

ch <- ch %>%
  group_by(id) %>%
  arrange(id, ordinal) %>%
  mutate(days.on.land = cumsum(land))

seven.days <- subset(ch, land == 1 & days.on.land > 6) # >= 7 days on land

seven.days.ids <- unique(seven.days$id) # list of bears that spend >=7 days on land
ch <- subset(ch, id %in% seven.days.ids) 

# fill in missing ordinal days with number; eh1 with 0 because animal not observed

ch <- ch %>%
  group_by(id) %>%
  complete(ordinal = 152:295, fill = list(eh1 = 0)) 


# Encounter history = 1 on occasion BEFORE animal is known to be swimming

ch <- ch %>% 
  group_by(id) %>%
  arrange(id, ordinal) %>%
  mutate(eh2 = if_else(lead(start.swim == 1), 1, 0))


  
  
 
         

 

ch <- ch %>%
  group_by(id) %>%
  distinct() %>%
  pivot_wider()

test <- ch %>%
  pivot_wider(
    names_from = ordinal,
    values_from = on.ice
  )
  
  


  
  


  