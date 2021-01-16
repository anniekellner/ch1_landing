##################################################################
##    CREATE DATAFRAME FOR FINAL COX MODEL   #####################
##################################################################

# January 14, 2021
# The final Cox model before submitting first draft to GW and SB

rm(list = ls())

library(dplyr)

# ----- LOAD DATA ----------------------------------------------------- #

bears <- readRDS('./data/RData/land_bears_CoxPH.Rds')

# ----  ADD DISTANCE TO PACK ICE -------------------------------------- #

# Wind

# Combine three separate Movebank spreadsheets (data obtained in separate requests)

wind1 <- read.csv("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Data/Movebank/Movebank_07032020.csv") 
wind2 <- read.csv("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Data/Movebank/Movebank_10082020.csv")
wind3 <- read.csv("C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Data/Movebank/Movebank_10192020.csv")

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

# ----  CREATE DF WITH DAILY AVERAGES  ------------- #

avg <- wind.join %>% # Compute daily average
  group_by(id, yday) %>%
  dplyr::summarise(
    first(animal), first(year), mean(dist2land), max(start.swim), mean(speed), mean(pland), mean(te), mean(dist_to_ice), first(repro), first(age), first(ResidualMass)) %>%
  ungroup()

colnames(avg) <- c("id", "ordinal_day", "animal", "SICsq", "SICmean", "SICmax", "SICmin", "MeanDist2land", "start.swim", "Windspeed", "pland", "te", "dist_to_ice") 

# remove rows after start.swim == 1

avg <- avg %>%
  group_by(id) %>%
  mutate(day = row_number()) 

avg <- avg %>% 
  group_by(id) %>%
  mutate(across(everything(),
                ~replace(., row_number() > match(1, start.swim), NA)))

# Add sd (rate of ice change)

avg$sd3 <- rollapplyr(avg$SICmean, 3, sd, fill = NA) # 7 is most descriptive but 3 

# ---------- CORRELATION MATRIX ------- #

correl <- avg[, 4:15]
mat <- cor(correl, use = "pairwise.complete.obs")

# -------   TMERGE TO CREATE TDC DATAFRAME -------------------------- #

temp <- subset(avg, start.swim == 1)
temp <- temp %>%
  dplyr::select(id, day, start.swim, animal) 

baseline <- tmerge(temp, temp, id = id, migrate = event(day, start.swim), tstart = 1, tstop = day)

cox_tdc <- tmerge(baseline, avg, id = id, 
                  SICmean = tdc(day, SICmean), 
                  SICmax = tdc(day, SICmax), 
                  SICmin = tdc(day, SICmin), 
                  SICsq = tdc(day, SICsq), 
                  sd7 = tdc(day, sd7), 
                  Distance_to_land = tdc(day, MeanDist2land), 
                  Windspeed = tdc(day, Windspeed), 
                  pland = tdc(day, pland), 
                  te = tdc(day, te), 
                  dist_to_ice = tdc(day, dist_to_ice))

saveRDS(cox_tdc, file = './data/RData/cox_tdc.Rds')
