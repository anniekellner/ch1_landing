rm(list = ls())

library(dplyr)
library(lubridate)

load("ows_land.RData")
load("pb_traj_df.RData")

refda <- parse_date_time(paste(min(ows.land$datetime)), orders = 'ymd HMS', tz = 'US/Alaska') #set reference date 

#calculate fix intervals
sum <- as.table(tapply(pb.traj.df$dt, pb.traj.df$burst, mean, na.rm=T))#mean dt for each burst
sum<- as.data.frame(sum)
colnames(sum) <- c("burst","dt_seconds")
sum <- mutate(sum, hours=dt_seconds/3600) #convert seconds to hours
#write.csv(sum, file="fix_intervals.csv")

#Divide into groups with same fix interval
pb.traj.df$burst <- as.character(pb.traj.df$burst)
one <- subset(pb.traj.df, burst== "pb_06810_2008" | burst=="pb_20414_2009" | burst=="pb_20446_2009" | burst=="pb_20520_2012"|
              burst=='pb_20586_2008' | burst=='pb_20735_2009' | burst=='pb_20741_2008' | burst=='pb_20845_2015' | burst=='pb_20965_2008'|
              burst=='pb_20966_2008'| burst=='pb_20975_2008'| burst=='pb_20982_2008'| burst=='pb_21343_2015'| burst=='pb_21380_2015'|
              burst=='pb_32255_2008'| burst=='pb_32282_2008' | burst=='pb_32608_2008'| burst== 'pb_32698_2015')

two <- subset(pb.traj.df, burst=="pb_20492_2008"| burst=="pb_20525_2013"| burst=="pb_20525_2014"| burst=="pb_20974_2008"|
              burst=="pb_21015_2013"| burst=="pb_21307_2012"| burst=="pb_21358_2013"| burst=="pb_21368_2014"| burst=="pb_32366_2014")

four <- subset(pb.traj.df, burst=="pb_20333_2008"| burst=="pb_20586_2009"| burst=="pb_20845_2009"| burst=="pb_21237_2011"|
               burst=="pb_21238_2011"| burst=="pb_21264_2011"| burst=="pb_32366_2011"| burst=="pb_06817_2006"| burst=="pb_20176_2005"|
                 burst=="pb_20413_2006"| burst=="pb_20418_2005"| burst=="pb_22129_2011")

#convert df to traj
library(adehabitatLT)
one$burst <- as.factor(one$burst)
one <- as.ltraj(one, date=one$date, id=one$burst, burst=one$burst)
two <- as.ltraj(two, date=two$date, id=two$burst, burst=two$burst)
four <- as.ltraj(four, date=four$date, id=four$burst, burst= four$burst)
  
length(unique(four$burst))
