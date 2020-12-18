###############################################
##    Figure: Departure Date Clustering   #####
###############################################

rm(list = ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

load("land_bears_CoxPH.RData")

start <- subset(bears, start.swim == 1)
start <- distinct(start)

start <- start[-16,]

start$ordinal <- yday(start$datetime)

table(start$year)

first <- min(start$ordinal)
last <- max(start$ordinal)

start$year <- as.numeric(start$year)
start$md <- 

# Remove years with < 2 observations

start <- start %>%
  filter(year != 2008) %>%
  filter( year != 2012) %>%
  filter(year != 2015) %>%
  select(animal, year, ymd, start.swim)

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
  geom_dotplot() +
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
    legend.position = c(0.05,0.5),
    #legend.text = element_text(size = 12),
    #legend.title = element_text(size = 14),
    #axis.title.x = element_text(vjust = -3)
    #axis.text.x = element_text(size = )
)
        
ggsave('depart_date_cluster.png', fig, path = './figures')  

