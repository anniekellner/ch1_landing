##################################################################
##    CREATE DATAFRAME FOR FINAL COX MODEL   #####################
##################################################################

# January 14, 2021
# The final Cox model before submitting first draft to GW and SB

rm(list = ls())

library(dplyr)
library(sf)
library(lubridate)
library(rWind)
library(zoo)
library(survival)
library(tidyr)

source('MyFunctions.R') # Turn Rds from sf object into dataframe

# ----- LOAD DATA ----------------------------------------------------- #

bears <- readRDS('./data/RData/land_bears_cutoff_after_swim.Rds')

bears <- st_drop_geometry(bears)

# Repro

bears$repro <- NA

bears <- bears %>%
  mutate(repro = replace(repro, go_into_den == 1, 1)) %>%
  mutate(repro = replace(repro, coy == 1, 2)) %>%
  mutate(repro = replace(repro, yearling == 1, 3))

bears$repro <- as.factor(bears$repro)

# Wind

# Combine three separate Movebank spreadsheets (data obtained in separate requests)

wind1 <- read.csv("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Chapter1/Data/Movebank/Movebank_07032020.csv") 
wind2 <- read.csv("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Chapter1/Data/Movebank/Movebank_10082020.csv")
wind3 <- read.csv("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Chapter1/Data/Movebank/Movebank_10192020.csv")

wind1 <- dplyr::select(wind1, timestamp, location.long, location.lat, ht.above.ellipsoid, id, NCEP.NARR.FLX.U.Wind.at.10.m.above.Ground, NCEP.NARR.FLX.V.Wind.at.10.m.above.Ground) # eliminate other environmental variables like Chinook parameter

wind <- rbind(wind1, wind2, wind3)
wind$timestamp <- ymd_hms(wind$timestamp, tz = "UTC") # datetime imported as factor from csv. Change to POSIXct object.

# Eliminate ice bears (from wind1)

lb <- unique(bears$id)
wind <- subset(wind, wind$id %in% lb)
wind <- distinct(wind)

colnames(wind) <- c("datetime", "gps_lon", "gps_lat", "ht_above_ellips", "id", "Wind_U", "Wind_V") # wind vectors NCEP NARR FLX
wind$datetime <- with_tz(wind$datetime, "US/Alaska")
wind$id.datetime <- paste(wind$id, wind$datetime)

wind.join <- inner_join(bears, wind)

# Remove id.datetime duplicates

wind.join <- wind.join %>%
  group_by(id.datetime) %>%
  slice_head()

rwind <- uv2ds(wind.join$Wind_U, wind.join$Wind_V)
rwind <- as.data.frame(rwind)

wind.join <- cbind(wind.join, rwind)
wind.join$yday <- yday(wind.join$datetime)

# Add wind direction

wind.join <- wind.join %>%
  mutate(wind_type = ifelse(dir > 135 & dir < 225, "Onshore",
                            ifelse(dir > 335 | dir < 45, "Offshore", "Crosswind")))

# ----  CREATE DF WITH DAILY AVERAGES  ------------- #

avg <- wind.join %>% # Compute daily average
  group_by(id, yday) %>%
  dplyr::summarise(
    first(animal), first(year), first(ResidualMass), mean(SIC_30m_me), min(SIC_30m_min), mean(dist_to_land), max(start.swim), mean(speed), max(speed), mean(dist_to_ice), first(wind_type)) %>%
  ungroup()

colnames(avg) <- c("id", "ordinal_day", "animal", "year", "ResidMass", "SIC_mean", "SIC_min", "dist_land", "start_swim", "speed_mean", "Speed_max", "dist_pack", "dir") 

# remove rows after start.swim == 1

avg <- avg %>%
  group_by(id) %>%
  mutate(day = row_number()) 

avg <- avg %>% 
  group_by(id) %>%
  mutate(across(everything(),
                ~replace(., row_number() > match(1, start_swim), NA)))

# Add sd (rate of ice change)

# Find mean of first value for sd

#sd <- avg %>%
 # group_by(id) %>%
  #slice_head(n = 7) %>%
  #drop_na(SIC_sd7)

#mean(sd$SIC_sd7)

avg <- avg %>%
  group_by(id) %>%
  mutate(SIC_sd7 = rollapplyr(SIC_mean, 7, sd, fill = 4.59)) # 7 is most descriptive. 4.59 is mean of first values of sd7 


# Change Distance values to km

avg$dist_land <- avg$dist_land / 1000
avg$dist_pack <- avg$dist_pack / 1000

# Add 3-day moving window and mean value for missing ResidMass

rm <- unique(avg$ResidMass)

mean(rm, na.rm = TRUE)

avg <- avg %>%
  group_by(id) %>%
  #mutate(SIC3 = rollapply(SIC, 3, mean, align = "right", partial = TRUE)) %>%
  mutate(speed3_mean = rollapply(speed_mean, 3, mean, align = "right", partial = TRUE)) %>%
  mutate(speed3_max_max = rollapply(Speed_max, 3, max, align = "right", partial = TRUE)) %>%
  mutate(speed3_max_mean = rollapply(Speed_max, 3, mean, align = "right", partial = TRUE)) %>%
  #mutate(dist_pack3 = rollapply(dist_pack, 3, mean, align = "right", partial = TRUE)) %>%
  #mutate(dist_land3 = rollapply(dist_land, 3, mean, align = "right", partial = TRUE)) %>%
  #mutate(te3 = rollapply(te, 3, mean, align = "right", partial = TRUE)) %>%
  replace_na(list(ResidMass = -8.384667)) # mean


# ---------- CORRELATION MATRIX ------- #

correl <- avg[, c(5:12,15,16)]
mat <- cor(correl, use = "pairwise.complete.obs")

# -------   TMERGE TO CREATE TDC DATAFRAME -------------------------- #

temp <- subset(avg, start_swim == 1)
temp <- temp %>%
  dplyr::select(id, day, start_swim, animal, year, ResidMass) 

baseline <- tmerge(temp, temp, id = id, migrate = event(day, start_swim), tstart = 1, tstop = day)

ph <- tmerge(baseline, avg, id = id, 
                  SIC_mean = tdc(day, SIC_mean), 
                  speed3_mean = tdc(day, speed3_mean), 
                  speed3_max_max = tdc(day, speed3_max_max),
                  speed3_max_mean = tdc(day, speed3_max_mean),
                  sd7 = tdc(day, SIC_sd7),
                  dist_land = tdc(day, dist_land), 
                  dist_pack = tdc(day, dist_pack),
             dir = tdc(day, dir))


mig <- subset(ph, migrate == 1) # Check values for day of migration
mig

saveRDS(ph, file = './data/RData/ph_Dec7.Rds')
