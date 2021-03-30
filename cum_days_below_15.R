###################################################
### CUMULATIVE TIME AT < 15% SIC ##################
###################################################

# Calculate for each bear the number of days <15%
# Calculate number of days as percentage of all days
# Plot distribution 
  # x-axis: time since at 15%
  # y-axis: cumulative % time <= 15%

#-------------------------------------------------#

rm(list = ls())

#library(plyr)
library(dplyr)
#library(data.table)
library(ggplot2)
library(stringr)
library(tidyr)

#load('all_v2.RData')
cox <- readRDS('./data/derived-data/avg.Rds') 

# Flag columns with <15% SIC

flag15 = cox %>% # flags columns with a 1 if SIC < 15
  group_by(id) %>%
  arrange(id, day) %>%
  mutate(flag15 = ifelse(SIC <15 ,1,0))

cum15 = flag15 %>% # creates columns that adds 1's by id
  group_by(id) %>%
  mutate(days_under15 = cumsum(flag15))

sum15 <- cum15 %>%
  group_by(id) %>%
  summarise(days = sum(days_under15))


sum15 <- sum15 %>% 
  separate(id, c("pb", "animal", "year")) %>%
  mutate(conc = "15")

sum15$conc <- 15

mean(sum15$days_under15)
min(sum15$days_under15)

# Days < 30%

flag30 = cox %>%
  group_by(id) %>%
  arrange(id, tstart) %>%
  mutate(flag30 = ifelse(SIC <30 ,1,0))

cum30 = flag30 %>%
  group_by(id) %>%
  mutate(days_under30 = cumsum(flag30))

sum30 <- cum30 %>%
  group_by(id) %>%
  slice_tail()

min(sum30$days_under30)
max(sum30$days_under30)
sd(sum30$days_under30)
  


sum30 <- sum30 %>% 
  separate(id, c("pb", "animal", "year"))

sum30$conc <- 30

# Days < 50


flag50 = cox %>%
  group_by(id) %>%
  arrange(id, tstart) %>%
  mutate(flag50 = ifelse(SIC <50, 1, 0))

cum50 = flag50 %>%
  group_by(id) %>%
  mutate(days_under50 = cumsum(flag50))

sum50 <- cum50 %>%
  group_by(id) %>%
  slice_tail()

min(sum50$days_under50)
max(sum50$days_under50)
sd(sum50$days_under50)
  
  summarise(days = sum(flag50)) 

sum50 <- sum50 %>% 
  separate(id, c("pb", "animal", "year"))

sum50$conc <- 50

# Combine results

tot <- rbind(sum15, sum30)
tot$conc <- as.factor(tot$conc)

# Get group means

mu <- ddply(tot, "conc", summarise, grp.mean = mean(days))

# Plot

gg <- ggplot(data = tot, aes(x = days, fill = conc)) + 
  geom_density(alpha = 0.4) + 
  geom_vline(data = tot, aes(xintercept))
  scale_x_continuous(limits = c(0,70), expand = c(0,0)) + 





gg15 <- ggplot(data = sum15, aes(days)) +
  geom_histogram(aes(y = ..density..), binwidth = 4, color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#FF6666") +
  scale_x_continuous(limits = c(0,70), expand = c(0,0)) + 
  xlab("Days spent at < 15% sea ice concentration") + 
  ylab("Density")

gg30 <- ggplot(data = sum, aes(days)) +
  geom_histogram(aes(y = ..density..), binwidth = 4, color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#FF6666") +
  scale_x_continuous(limits = c(0,70), expand = c(0,0)) + 
  xlab("Days spent at < 15% sea ice concentration") + 
  ylab("Density")



ggsave(filename = 'Days_below_15.png', path = './figures')


#saved C:\Users\akell\Desktop\Spring 2019\Presentations\Alaska\days_15pct.pdf

#------------- MEAN AND STDEV------------------------------------#

last <- ice.calc %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice(n())

mean(sum$days, na.rm = TRUE) # 22.72 days
sd(sum$days, na.rm = TRUE) # 13.15 days 


