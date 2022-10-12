###############################################
##    Figure: Departure Date Clustering   #####
###############################################

rm(list = ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(sf)

source('MyFunctions.R')

# ------  LOAD DATA --------------------------------- #

ph <- readRDS('./data/RData/land_bears_cutoff_after_swim.Rds') # loads as sf object
ph <- st_drop_geometry(ph)

# ---   FORMAT  ----------------------------------- #

start <- filter(ph, start.swim == 1)

start$ordinal <- yday(start$datetime)

start$year <- as.Date(as.character(start$year), format = "%Y")
start$year <- year(start$year)

start2 <- start %>%
  dplyr::select(animal, year, ordinal) %>%
  mutate(diff = year - 2005)

start2$year<- as.factor(start2$year)

# ------- PLOT  ------------------------------- #

fig3 <- ggplot(data = start2, aes(x = year, y = ordinal)) + 
  geom_point(size = 2) + 
  xlab("\nYear") +
  ylab("Ordinal Date of Departure\n") + 
  theme_classic()

width <- 10 # in px, looks nearly square. Change if journal gives different size
height <- 10
path <- 'C:/Users/akell/OneDrive - Colostate/PhD/Chapter1/Dissertation/Ecosphere/Publication/Figures'

# Save as TIFF as recommended by Ecosphere

#ggsave(filename = 'Kellner_et_al_2022_Figure3.tif', plot =fig3, device = 'tiff', path = path, width = width, height = height, dpi = 600)


################ ASSUMPTIONS  #############################################

# Does the data meet the assumptions for linear regression?

# Test for normality

swim$ordinal <- yday(swim$ymd)

shapiro.test(swim$ordinal) # data does not deviate from a normal distribution
