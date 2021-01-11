######################################################
###   ADD BIOLOGICAL VARIABLES TO COX DATABASE  ######
######################################################

library(dplyr)


rm(list = ls())

# Prep data

load('Cox_TDC.RData')
bio1 <- read.csv('./data/raw-data/resid_mass_indfem_19832015.csv')
bio2 <- read.csv('./data/raw-data/Residual_body_masses_additional_bears_20210102.csv')

bio1 <- select(bio1, BearID, capture.year, age, ResidualMass)

bio1$animal <- paste0("pb_", bio1$BearID)
bio1$id <- paste(bio1$animal, bio1$capture.year, sep = ".")

bio1 <- bio1 %>%
  select(-c(BearID, capture.year))

bio2$id <- paste(bio2$ï..animal, bio2$CapYr, sep = '.')

bio2 <- bio2 %>%
  select(-c(Comment, CapYr)) 

colnames(bio2) <- c("animal", "ResidualMass", "age", "id")

biolog <- rbind(bio1, bio2)

cox_tdc <- left_join(cox_tdc, biolog)

cox_tdc %>%
  group_by(id) %>%
  slice_head()

missing <- subset(bio, animal == "pb_20845") # no residmass but other measurements present - 20446
# 6817, 20333 (2008), 20418 (2005), 20520 (2012), 20529 (2005), 20735 (2009), 21264 (2011), 21358 (2013) 


# ------------------------------------------------------------------------------------------------------- #
 # COX REGRESSION WITH BIOLOGICAL VARIABLES #

create_AICc_table <- function(aicc){ # AICc table function
  aicc$Model <- rownames(aicc)
  aicc$deltaAICc <- aicc$AICc - min(aicc$AICc) 
  aicc$L <- exp(-0.5*(aicc$deltaAICc))
  aicc$weight <- aicc$L/(sum(aicc$L))
  aicc$weight.pct <- aicc$weight*100
  aicc <- arrange(aicc, aicc$deltaAICc)
  return(aicc)
}

