#################################
##    LOGISTIC REGRESSION #######
#################################

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

# Load dataframe #

load('Ice_Measurements.RData')
load('lsm.RData')

ice.df <- dplyr::select(ice.df, animal:datetime, start.swim, id:ord.year)
lsm$index <- as.numeric(lsm$index)

# subset lsm by radius

lsm10 <- dplyr::filter(lsm, radius_m == 10000) # 10 km radius
lsm30 <- dplyr::filter(lsm, radius_m == 30000) # 30 km radius

###############################################################################
###     MEAN PATCH AREA   #####################################################

### 10 km Radius  ###

area_mn_10 <- lsm10 %>%
  filter(metric == "area_mn") %>%
  filter(class == 3)

head(area_mn_10)
area_mn_10$id.datetime <- paste(area_mn_10$id, area_mn_10$datetime)

area_mn_10 <- area_mn_10 %>% pivot_wider(names_from = c(class), values_from = c(class, value)) # spread rows into columns by class


full <- left_join(ice.df, area_mn_10, by = "id.datetime")# join ice.df with lsm df's

# Change ice value (3) to 0 if NA
full$value_3[is.na(full$value_3)] <- 0
full$class_3[is.na(full$class_3)] <- 3

full$leave.ice <- ifelse(full$index==0,1,0)

# logistic regression swim ~ value raw data
fit <- glm(start.swim ~ value_3, data = full, family = binomial())
summary(fit) 

# find means by days

mean_val <- full %>% 
  group_by(id.x, index) %>% 
  summarise(mean_val = mean(value_3)) 
  
sum_swim <- full %>%
  group_by(id.x, index) %>%
  summarize(swim = max(start.swim))
  
logreg <- left_join(mean_val, sum_swim)   

# Log. Reg. swim - averaged over 30 days
fit2 <- glm(swim ~ mean_val, data = logreg, family = binomial())
summary(fit2)

######   30 km radius

area_mn_30 <- lsm30 %>%
  filter(metric == "area_mn") %>%
  filter(class == 3)

head(area_mn_30)
area_mn_30$id.datetime <- paste(area_mn_30$id, area_mn_30$datetime)

area_mn_30 <- area_mn_30 %>% pivot_wider(names_from = c(class), values_from = c(class, value)) # spread rows into columns by class


full <- left_join(ice.df, area_mn_30, by = "id.datetime") # join ice.df with lsm df's

# Change ice value (3) to 0 if NA
full$value_3[is.na(full$value_3)] <- 0
full$class_3[is.na(full$class_3)] <- 3

# logistic regression swim ~ value raw data
fit <- glm(start.swim ~ value_3, data = full, family = binomial())
summary(fit) 

# find means by days

mean_val <- full %>% 
  group_by(id.x, index) %>% 
  summarise(mean_val = mean(value_3)) 

sum_swim <- full %>%
  group_by(id.x, index) %>%
  summarize(swim = max(start.swim))

logreg <- left_join(mean_val, sum_swim)   

# Log. Reg. swim - averaged over 30 days
fit2 <- glm(swim ~ mean_val, data = logreg, family = binomial())
summary(fit2)

ggplot(logreg, aes(mean_val, swim)) + geom_point() +
  scale_x_reverse() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

par(mar = c(4,4,1,1))


######  TROUBLESHOOTING     #################################################################################

## RESOLVED 04/01/2020
# which ice.df animals do not have start.swim date 

load('all.v2.RData')
load('ded_ids.RData')

test <- subset(ice.df, start.swim ==1) # 9 observations
start.swim <- subset(all.v2, start.swim==1) # 17 observations
ded # 14 observations: minus year < 2006 --> should be 12
ice.df$id # 12 observations

nodata <- ice.df$id[!(ice.df$id %in% test$id)] 
unique(nodata) # ANSWER: 20525.2013, 20414.2009, 20446.2009

# 20525 - start swim date: 8/17/13 00:00:13

pb20525.ice <- subset(ice.df, animal=="pb_20525")


  