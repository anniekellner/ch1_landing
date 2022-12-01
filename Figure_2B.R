###################################################
###   FIGURE 2b  ##################################
###################################################

# MEAN ICE OVER ENTIRE SHELF  

# icemean = mean ice concentration over shelf

rm(list = ls())

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# ---- Load and prep data ----------------------- #

ice <- read.csv('./data/raw-data/sbs_daily_icestats_1979_2017_v6.csv')

ice$date <- mdy(ice$date)

ice <- ice %>%
  select(date, contains("icemean"))

ice <- select(ice, date, contains("shelf")) 

ice <- ice %>%
  mutate(month = month(ice$date)) %>%
  mutate(year = year(ice$date)) %>%
  mutate(day = day(ice$date)) %>%
  mutate(ordinal = yday(ice$date))

ice <- ice %>%
  filter(month > 5 & month < 10) %>%
  filter(year > 2004 & year < 2016) %>%
  select(1:2,5:8) %>%
  rename(mean_conc = sbs_shelf_icemean_15)

ice$year <- as.factor(ice$year)

#ice <- filter(ice, year != 2007 | year != 2010)

over50 <- ice %>%
  group_by(year) %>%
  mutate(flag = ifelse(mean_conc >= 50, 1, 0)) %>%
  mutate(cum = cumsum(flag)) %>%
  slice_tail() %>%
  select(year, cum) %>%
  rename(daysOver50 = cum)

under50over30 <- ice %>%
  group_by(year) %>%
  mutate(flag = ifelse(mean_conc <50 & mean_conc>=30, 1, 0)) %>%
  mutate(cum = cumsum(flag)) %>%
  slice_tail() %>%
  select(year, cum) %>%
  rename(daysUnder50over30 = cum) 

under30over15 <- ice %>%
  group_by(year) %>%
  mutate(flag = ifelse(mean_conc <30 & mean_conc>=15, 1, 0)) %>%
  mutate(cum = cumsum(flag)) %>%
  slice_tail() %>%
  select(year, cum) %>%
  rename(daysUnder30over15 = cum) 

under15 <- ice %>%
  group_by(year) %>%
  mutate(flag = ifelse(mean_conc < 15, 1, 0)) %>%
  mutate(cum = cumsum(flag)) %>%
  slice_tail() %>%
  select(year, cum) %>%
  rename(daysUnder15 = cum)

shelf <- over50 %>%
  left_join(under50over30) %>%
  left_join(under30over15) %>%
  left_join(under15)

shelf.long <- shelf %>%
  pivot_longer(!year, names_to = "concentration", values_to = "days")

# Order factor levels

shelf.long$concentration <- factor(shelf.long$concentration, levels = c("daysOver50", "daysUnder50over30", "daysUnder30over15", "daysUnder15"))

gg_shelf_ice <- ggplot(data = shelf.long, aes(x = year, y = days, fill = concentration)) +
  geom_bar(alpha = 0.5, position = "stack", stat = "identity") + 
  scale_fill_discrete(name = "Concentration",labels = c("> 50%", "30 - 50%", "15 - 30%", "< 15%")) + 
  scale_y_continuous(limits = c(0, 125), expand = c(0,0)) +
  ylab("Days in the Open Water Season") + 
  xlab("Year") +
  theme_classic()

#ggsave("Fig2B.pdf", plot = gg_shelf_ice, path = 'C:/Users/akell/OneDrive - Colostate/PhD/Chapter1/Dissertation/Figures')
