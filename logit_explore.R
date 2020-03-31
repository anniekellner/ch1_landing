#################################
##    LOGISTIC REGRESSION #######
#################################

rm(list = ls())

library(dplyr)

# Load dataframe #

load('Ice_Measurements.RData')
load('lsm.RData')

ice.df <- select(ice.df, animal:datetime, start.swim, id:ord.year)

# subset lsm by radius

lsm10 <- dplyr::filter(lsm, radius_m == 10000) # 10 km radius
lsm30 <- dplyr::filter(lsm, radius_m == 30000) # 30 km radius

## Mean Patch Area ##
area_mn_10 <- lsm10 %>%
  filter(metric == "area_mn") %>%
  filter(class == 3)

head(area_mn_10)
area_mn_10$id.datetime <- paste(area_mn_10$id, area_mn_10$datetime)

test <- setdiff(ice.df$datetime, area_mn_10$datetime)

pb1_ice <- subset(ice.df, animal == "pb_06817")
pb1_area <- subset(area_mn_10, id == "pb_06817.2006")

test <- subset(ice.df, !(datetime %in% area_mn_10$datetime))


test <- full_join(ice.df, area_mn_10)



test <- pivot_wider(area_mn_10, plot_id, names_from = metric, values_from = value)
