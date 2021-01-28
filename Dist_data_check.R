###########################################
##    TEST DISTANCE MEASUREMENTS ##############
###########################################

library(ggplot2)
library(plotly)
library(dplyr)

# DISTANCE TO PACK

# Check ten randomly selected values against distance to pack measurements
# Forgot to set.seed

mydata <- readRDS('./data/RData/land_bears_cutoff_after_swim.Rds')

x <- 1:13447

ver <- sample(x, size = 10, replace = FALSE)

# 4270 = TRUE
# 12935 = TRUE
# 7655 = TRUE
# 8399 = TRUE
# 1352 = TRUE
# 517 =  TRUE
# 1380 = TRUE
# 2000 = TRUE
# 11827 = TRUE
# 11719 = TRUE

set.seed(13)
ver <- sample(x, size = 10, replace = FALSE)

# 9152 = TRUE

## DISTANCE TO LAND

set.seed(118)
ver <- sample(x, size = 10, replace = FALSE)

# 3072 = TRUE
# 4363 = TRUE
# 10219 = TRUE

# SIC 

# covariance between pland and SIC: 0.71

a <- mydata$SIC_30m_me
b <- mydata$pland

var(a,b, na.rm = TRUE)
cor(a,b, use = "na.or.complete")

# Find outliers in correlation plot

p <- ggplot(data = mydata, aes(SIC_30m_me, pland, color = id.ymd)) + 
  geom_point()

ggplotly(p)

sub <- filter(mydata, id == "pb_21368.2014")
