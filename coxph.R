########################################################################
####    COX PROPORTIONAL HAZARD REGRESSION    ##############################
########################################################################

rm(list = ls())

library(survival)
library(dplyr)

# ------- FUNCTIONS ------------------------------------------------- #

create_AICc_table <- function(aicc){ # AICc table function
  aicc$Model <- rownames(aicc)
  aicc$deltaAICc <- aicc$AICc - min(aicc$AICc) 
  aicc$L <- exp(-0.5*(aicc$deltaAICc))
  aicc$weight <- aicc$L/(sum(aicc$L))
  aicc$weight.pct <- aicc$weight*100
  aicc <- arrange(aicc, aicc$deltaAICc)
  return(aicc)
}

# ------- LOAD AND PREP DATA ----------------------------------------------------------- #



load("logreg.RData")

logreg <- select(logreg, -geometry)

avg <- logreg %>% 
  group_by(id, ymd) %>%
  dplyr::summarise(
    first(animal), mean(SIC_30m_me), mean(SIC_30m_max), mean(SIC_30m_min), mean(SICsq), mean(dist2land[,1]), max(start.swim), max(Bonepile)
  )

colnames(avg) <- c("id", "datetime", "animal", "SIC_mean", "SIC_max", "SIC_min", "SICsq", "Dist2land", "start.swim", "Bonepile")

avg <- avg %>%
  group_by(id) %>%
  mutate(tstart = row_number()) %>%
  mutate(tstop = lead(tstart))

load("ded_ids.RData")

sub <- subset(avg, id %in% ded) # subset land bears 

sub <- sub %>%
  group_by(id) %>%
  mutate(across(starts_with('t'),
                ~replace(., row_number() > match(1, start.swim), NA)))


fit <- coxph(Surv(tstart, tstop, start.swim) ~ SIC_mean, data = sub)



aicc <- AICc(fit_max, fit_SIC_mean, fit_SIC_min, fit_SIC_sq)

create_AICc_table(aicc)


testplot <- survfit(fit)
plot(testplot)
