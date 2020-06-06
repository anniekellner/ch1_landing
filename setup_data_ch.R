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
lb <- land.bears.all.ows

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
    df <- select(df, id, ymd, ordinal, on.ice, land)
  return(df)
}

ch <- prep_data(lb)

# ---   Format capture history (ch) for known-fate models ----------------------- #


# Create capture histories
# https://jamesepaterson.github.io/jamespatersonblog/07_creatingcapturehistories
# Chapter 16, Cooch & White 2020

 
ch$eh1 <- 1 # all recorded observations = 1 (first number in paired ch)

ch <- ch %>% # if there were both on and off land observations in a single day, took first
  group_by(id, ordinal) %>%
  slice(1) %>%
  distinct() # remove duplicates

ch <- ch %>%
  group_by(id) %>%
  arrange(id, ordinal) %>%
  mutate(days.on.land = cumsum(land))

ch <- ch %>%
  group_by(id) %>%
  complete(ordinal = 152:295, fill = list(eh1 = 0)) # fill in missing ordinal days with number; eh1 with 0 because animal not observed




ch %>%
  group_by(id) %>%
  mutate(flag = )
  

land.bears <- ows %>% # create column for number of days spent on land by individual bear
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(days.on.land = cumsum(land))


  
  mutate(eh1 = if_else(ordinal == 152 |  )
 

ch <- ch %>%
  group_by(id) %>%
  distinct() %>%
  pivot_wider()

test <- ch %>%
  pivot_wider(
    names_from = ordinal,
    values_from = on.ice
  )
  
  
  mutate(eh1 = if_else(ordinal == 152) | lead(on.ice == 1)) %>%

  
  
  x= flag.15 %>%
  group_by(id) %>%
  arrange(id,datetime) %>%
  mutate(time.15 = ifelse(flag.15==0 | lag(flag.15)==0,
                          0, difftime(datetime, lag(datetime), units='days'))) %>%
  mutate(cumtime.15 = cumsum(ifelse(is.na(time.15), 0, time.15)) + time.15*0) %>%
  mutate(pct.days.below15 = cumtime.15/30) %>%
  mutate(index=difftime(last(ymd), ymd, units='days')) 
  

  