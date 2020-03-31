#################################
##    LOGISTIC REGRESSION #######
#################################

rm(list = ls())

library(dplyr)
library(tidyr)

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

area_mn_10 <- area_mn_10 %>% pivot_wider(names_from = c(class), values_from = c(class, value)) # spread rows into columns by class

full <- left_join(ice.df, area_mn_10, by = "id.datetime") # join ice.df with lsm df's
                    