###############################################
##    Figure: Departure Date Clustering   #####
###############################################

rm(list = ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(cowplot)

load("land_bears_CoxPH.RData")

start <- subset(bears, start.swim == 1)
start <- distinct(start)

start <- start[-16,]

start$ordinal <- yday(start$datetime)

table(start$year)

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
  geom_dotplot(stackgroups = TRUE) +
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

fit <- lm(ordinal ~ year, data = start) # linear regression
summary(fit)

new <- data.frame(year =  c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014))

new$start <- predict(fit, newdata = new)
new$year <- as.factor(new$year)

# Create dataframe to predict over to show regression line on plot (otherwise intercept is nonsensical)

start$year<- as.factor(start$year)

plot_reg <- ggplot(data = start, aes(year, ordinal)) + 
  geom_point(size = 3, show.legend = FALSE) + 
  xlab("Year") + 
  ylab("Ordinal Date") + 
  geom_line(data = new, aes(year, start), group = 1, color = "black", linetype = "dashed")


ggsave('depart_date_regress.png', plot_reg, path = './figures', dpi = 300)
