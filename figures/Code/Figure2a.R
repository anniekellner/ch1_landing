########################################################
###   FIGURE 2A   ######################################
########################################################

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
library(viridis)

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
  geom_vline(data = mu, aes(xintercept = grp.mean, color = conc), linetype = "dashed", show.legend = FALSE) + # vline colors are wrong but can change in AI
  scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_fill_viridis(name = "Concentration",labels = c("> 50%", "30 - 50%", "15 - 30%", "< 15%"), discrete = TRUE) +
  scale_y_continuous(limits = c(0, 0.15), expand = c(0,0)) + 
  xlab("Days") + 
  ylab("Density") + 
  theme_classic()

#ggsave("Fig2A.pdf", plot = gg, path = 'C:/Users/akell/OneDrive - Colostate/PhD/Chapter1/Dissertation/Figures')
