
rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

load('lsm.RData')

# prep data - all

lsm <- separate(lsm.df, col = "plot_id", into = c("id", "date", "time"), sep = " ", remove = FALSE)
lsm$datetime <- paste(lsm$date, lsm$time); lsm$datetime <- as.POSIXct(lsm$datetime) # add new column for datetime
lsm$date <- ymd(lsm$date)
lsm <- lsm %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(index = difftime(last(date), date, units = "days"))

# FIGURE OUT HOW TO ADD 0'S FOR DAYS WITH CLASS 0 ONLY (NO DATA FOR CLASS = 3)

test <- subset(lsm10, id == 'pb_21015.2013')
test <- subset(test, metric == "area_mn")



head(test)

test %>% complete(index, class, fill = list(value = 0)) # HOLY SHIT THIS WORKS!!!!!!


# Subset
 
lsm10 <- dplyr::filter(lsm, radius_m == 10000) # 10 km radius

area_mn_10 <- lsm10 %>%
  filter(metric == "area_mn") %>%
  filter(class == 3)

# plot

ggplot(area_mn_10, aes(index, value, color = id, na.rm = TRUE)) +
  stat_summary(geom = 'line', fun = 'mean') +
  scale_x_reverse() +
  xlab("Time") +
  ylab("Mean Area") +
  ggtitle("Mean Area of Ice Patch") +
  theme_bw()
 





