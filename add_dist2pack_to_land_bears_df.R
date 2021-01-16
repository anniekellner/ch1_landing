###################################################
####      ADD DISTANCE TO PACK ICE TO MODEL #######
###################################################

rm(list = ls())

# --- DATA ---------------------------------------------------------------- #

# Bear data

bears <- readRDS('./data/RData/land_bears_CoxPH.Rds')
bears <- distinct(full)

remove <- filter(bears, year == 2012 & month == 6 | ymd == '2012-07-01' | ymd == '2012-07-02') # observations that do not have associated shp's
bears <- anti_join(bears, remove)

bears1 <- bears[1:18123,]
bears2 <- bears[18124:24157,]

load('./data/RData/dist1.RData')
dist1 <- dist

bears1$dist_to_ice <- dist1

load('./data/RData/dist2.RData')
dist2 <- dist

bears2$dist_to_ice <- dist2

bears3 <- rbind(bears1, bears2)
head(bears3)
tail(bears3)

bears3 <- bears3 %>%
  group_by(id.datetime) %>%
  slice_head()

saveRDS(bears3, file = './data/RData/land_bears_CoxPH.Rds')

