#########################################
##    Figure 2: Days at SIC #############
#########################################

# Draft 4 Fig 2: showing how many days bears spend at varying SIC's

library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())

cox <- readRDS('./data/derived-data/avg.Rds') 

over50 <- cox %>%
  group_by(year) %>%
  mutate(flag = ifelse(SIC > 50, 1, 0)) %>%
  mutate(cum = cumsum(flag)) %>%
  slice_tail() %>%
  select(year, cum) %>%
  rename(daysover50 = cum)
      
under50over30 <- cox %>%
  group_by(year) %>%
  mutate(flag = ifelse(SIC <50 & SIC >=30, 1, 0)) %>%
  mutate(cum = cumsum(flag)) %>%
  slice_tail() %>%
  select(year, cum) %>%
  rename(daysUnder50over30 = cum) 

under30over15 <- cox %>%
  group_by(year) %>%
  mutate(flag = ifelse(SIC <30 & SIC>=15, 1, 0)) %>%
  mutate(cum = cumsum(flag)) %>%
  slice_tail() %>%
  select(year, cum) %>%
  rename(daysUnder30over15 = cum) 

under15 <- cox %>%
  group_by(year) %>%
  mutate(flag = ifelse(SIC < 15, 1, 0)) %>%
  mutate(cum = cumsum(flag)) %>%
  slice_tail() %>%
  select(year, cum) %>%
  rename(daysUnder15 = cum)

all <- over50 %>%
  left_join(under50over30) %>%
  left_join(under30over15) %>%
  left_join(under15)

all.long <- all %>%
  pivot_longer(!year, names_to = "concentration", values_to = "days")

head(all.long)

# Order factors

all.long$concentration <- factor(all.long$concentration, levels = c("daysover50", "daysUnder50over30", "daysUnder30over15", "daysUnder15"))
all.long$year <- as.factor(all.long$year) # so all years appear on plot (with no gaps)

gg_SIC <- ggplot(data = all.long, aes(x = year, y = days, fill = concentration)) +
  geom_bar(alpha = 0.5, position = "stack", stat = "identity") + 
  scale_fill_discrete(name = "Concentration",labels = c("> 50%", "30 - 50%", "15 - 30%", "< 15%")) + 
  ylab("Days") + 
  xlab("Year") 

gg_SIC


ggsave(gg_SIC, filename = './figures/days_at_SIC_by_year.png')
