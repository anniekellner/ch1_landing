###############################################
##    Figure: Departure Date Clustering   #####
###############################################

rm(list = ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(cowplot)
library(sf)

ph <- readRDS('./data/RData/land_bears_cutoff_after_swim.Rds')

source('MyFunctions.R')

start <- filter(ph, start.swim == 1)

table(start$year)

start$ordinal <- yday(start$datetime)

first <- min(start$ordinal)
last <- max(start$ordinal)

start$year <- as.numeric(start$year)

# Remove years with < 2 observations

start <- start %>%
  filter(year != 2008) %>%
  filter( year != 2012) %>%
  filter(year != 2015) %>%
  select(animal, year, ymd, ordinal, start.swim)

# Convert date to month-day

start$md <- ymd(start$ymd)

as.mmdd <- function(x, ...) UseMethod("as.mmdd")
as.mmdd.Date <- function(x, ...) structure(x, class = c("mmdd", "Date"))

as.Date.mmdd <- function(x, ...) structure(x, class = "Date")

format.mmdd <- function(x, format = "%m-%d", ...) format(as.Date(x), format = format, ...)

start2 <- transform(start, md = as.mmdd(md))

start2$dummy_year <- 2000

start2$md2000 <- paste(start2$dummy_year, start2$md, sep = "-")
start2$md2000 <- ymd(start2$md2000)

start2$year <- as.factor(start2$year)

# Plot

fig <- ggplot(data = start2, aes(x = md2000, col = year, fill = year)) + 
  geom_dotplot(stackgroups = TRUE, binpositions = "all") +
  scale_color_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  xlab("\nDate of Departure from Ice") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = c(0.05,0.5)
)
        
ggsave('depart_date_cluster.png', fig, path = './figures')  

## Trend over time

start <- st_drop_geometry(start)
start$year <- as.Date(as.character(start$year), format = "%Y")
start$year <- year(start$year)

start2 <- start %>%
  dplyr::select(animal, year, ordinal) %>%
  mutate(diff = year - 2005)


fit <- lm(ordinal ~ diff + diff, data = start2) # linear regression
summary(fit)

new <- data.frame(year =  c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015))
new <- data.frame(diff = seq(0,10,1))

new$start <- predict(fit, newdata = new)
#new$year <- as.factor(new$year)

# Create dataframe to predict over to show regression line on plot (otherwise intercept is nonsensical)

#start$year<- as.factor(start$year)
#start$ymd <- ymd(start$ymd)

plot_reg <- ggplot(data = start2, aes(x = diff, y = ordinal)) + 
  geom_point(size = 3, show.legend = FALSE) + 
  scale_y_continuous(limits = c(182, 274), breaks = c(182, 213, 244, 274)) +
  xlab("Year") + 
  ylab("Day of the Year") + 
  geom_line(data = new, aes(diff, start), group = 1, color = "black", linetype = "dashed") + 
  theme_bw()


ggsave('depart_date_regress.png', plot_reg, path = './figures', dpi = 300)

# Does the data meet the assumptions for linear regression?

# Test for normality

swim$ordinal <- yday(swim$ymd)

shapiro.test(swim$ordinal) # data does not deviate from a normal distribution
