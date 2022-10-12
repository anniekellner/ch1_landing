####################################################### 
###   FIGURE - DOTPLOT TO SHOW DEPARTURE DATES  #######
#######################################################

# For presentations only

# Copied and pasted from figure_cluster_depart_date
# If anything doesn't work, check with that script

rm(list = ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(sf)

source('MyFunctions.R')

# ------  LOAD DATA --------------------------------- #

ph <- readRDS('./data/RData/land_bears_cutoff_after_swim.Rds') # loads as sf object
ph <- st_drop_geometry(ph)

# ----- PREP START DATE -------------------------- #

start <- filter(ph, start.swim == 1)

table(start$year)

start$ordinal <- yday(start$datetime)

#first <- min(start$ordinal)
#last <- max(start$ordinal)

start$year <- as.numeric(start$year)

# Separate years with < 2 observations - keep if making figs for presentations

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
  scale_color_brewer(palette = "Set3") + 
  scale_fill_brewer(palette = "Set3") + 
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

#ggsave('depart_date_cluster.svg', fig, path = './figures')  