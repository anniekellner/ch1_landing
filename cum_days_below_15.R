###################################################
### CUMULATIVE TIME AT < 15% SIC ##################
###################################################

# Calculate for each bear the number of days <15%
# Calculate number of days as percentage of all days
# Plot distribution 
  # x-axis: time since at 15%
  # y-axis: cumulative % time <= 15%

## NOTE: If this script give errors, exist out of RStudio and try again. Could be an issue with orders of package loading (ie plyr before dplyr)
#-------------------------------------------------#

rm(list = ls())

library(plyr)
library(dplyr)
#library(data.table)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)

#load('all_v2.RData')
cox <- readRDS('./data/derived-data/avg.Rds') 

# Flag columns with <15% SIC

flag15 = cox %>% # flags columns with a 1 if SIC < 15
  group_by(id) %>%
  arrange(id, day) %>%
  mutate(flag15 = ifelse(SIC < 15 ,1,0))

cum15 = flag15 %>% # creates columns that adds 1's by id
  group_by(id, day) %>%
  mutate(days_under15 = cumsum(flag15))

sum15 <- cum15 %>% # Add flags
  group_by(id) %>%
  summarise(days = sum(days_under15))

sum15 <- sum15 %>% # Separate id into three columns
  separate(id, c("pb", "animal", "year")) %>%
  mutate(conc = " < 15")

# For descriptive purposes

mean(sum15$days)
min(sum15$days)
max(sum15$days)
sd(sum15$days)
# Days < 30%

flag30 = cox %>% # flags columns with a 1 if SIC < 30
  group_by(id) %>%
  arrange(id, day) %>%
  mutate(flag30 = ifelse(SIC < 30 & SIC >= 15 ,1,0))

cum30 = flag30 %>% # creates columns that adds 1's by id
  group_by(id, day) %>%
  mutate(days_15to30 = cumsum(flag30))

sum30 <- cum30 %>% # Add flags
  group_by(id) %>%
  summarise(days = sum(days_15to30))

sum30 <- sum30 %>% # Separate id into three columns
  separate(id, c("pb", "animal", "year")) %>%
  mutate(conc = "15 - 30")

mean(sum30$days)
min(sum30$days)
max(sum30$days)
sd(sum30$days)
  

# Days < 50

flag50 = cox %>% # flags columns with a 1 if SIC < 50
  group_by(id) %>%
  arrange(id, day) %>%
  mutate(flag50 = ifelse(SIC <50 & SIC >= 30 ,1,0))

cum50 = flag50 %>% # creates columns that adds 1's by id
  group_by(id, day) %>%
  mutate(days_30to50 = cumsum(flag50))

sum50 <- cum50 %>% # Add flags
  group_by(id) %>%
  summarise(days = sum(days_30to50))

sum50 <- sum50 %>% # Separate id into three columns
  separate(id, c("pb", "animal", "year")) %>%
  mutate(conc = "30 - 50")

mean(sum50$days)
min(sum50$days)
max(sum50$days)
sd(sum50$days)

# Days >50

flag100 = cox %>% # flags columns with a 1 if SIC < 50
  group_by(id) %>%
  arrange(id, day) %>%
  mutate(flag100 = ifelse(SIC > 50,1,0))

cum100 = flag100 %>% # creates columns that adds 1's by id
  group_by(id, day) %>%
  mutate(days_over50 = cumsum(flag100))

sum100 <- cum100 %>% # Add flags
  group_by(id) %>%
  summarise(days = sum(days_over50))

sum100 <- sum100 %>% # Separate id into three columns
  separate(id, c("pb", "animal", "year")) %>%
  mutate(conc = "> 50")

mean(sum100$days)
min(sum100$days)
max(sum100$days)
sd(sum100$days)

# Combine results

tot <- rbind(sum15, sum30)
tot <- rbind(tot, sum50)
tot <- rbind(tot, sum100)
tot$conc <- factor(tot$conc, levels = c("> 50", "30 - 50", "15 - 30", " < 15"))

# Get group means

mu <- ddply(tot, "conc", summarise, grp.mean = mean(days))

# Plot

gg <- ggplot(data = tot, aes(x = days, fill = conc)) + 
  geom_density(alpha = 0.2) + 
  geom_vline(data = mu, aes(xintercept = grp.mean, color = conc), linetype = "dashed", show.legend = FALSE) +
  scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_fill_discrete(name = "Concentration",labels = c("> 50%", "30 - 50%", "15 - 30%", "< 15%")) +
  scale_y_continuous(limits = c(0, 0.15), expand = c(0,0)) + 
  xlab("Days") + 
  ylab("Density")

 

ggsave(gg, file = './figures/days_on_ice.png')

# ------------  MEAN ICE OVER ENTIRE SHELF  -------------------------------------------------------- #

# icemean = mean ice concentration over shelf

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
  xlab("Year") 

ggsave(gg_shelf_ice, filename = './figures/mean_shelf_ice.png')
