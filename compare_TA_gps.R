rm(list = ls())

load('all.RData')
todd <- read.csv("SB_paths_landsummary_1986-2012_for KL.csv")

#compare TA list with all.RData to see which points have gps data
all$tt <- paste(all$animal, all$year, sep='.')
tt <- unique(all$tt)

todd$gps <- ifelse(todd$ï..id %in% tt,1,0)

ta.gps <- subset(todd, gps==1)

write.csv(ta.gps, file="ta_gps.csv")

unique(ta.gps$ï..id)


