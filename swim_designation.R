rm(list = ls())
load('all.RData')
library(dplyr)
library(lubridate)
swim <- read.csv('ice_arrive_depart.csv')

### Add 1's to 'swim' column in all.RData


#select relevant columns and reformat times to POSIXct
colnames(swim) <- c("Animal","Year", "Dep.Ice","Time.Dep", "Arr.Land", "Time.Arr", "Notes") #rename columns
swim <- select(swim, Animal:Time.Arr)

swim$Dep.Ice <- as.Date(swim$Dep.Ice, '%m/%d/%Y') # change ambiguous format
swim$Dep.Ice <- ymd(swim$Dep.Ice, tz='US/Alaska')
swim$Arr.Land <- as.Date(swim$Arr.Land, '%m/%d/%Y')
swim$Arr.Land <- ymd(swim$Arr.Land, tz='US/Alaska')

swim$start.datetime <- as.POSIXct(paste(swim$Dep.Ice, swim$Time.Dep), tz='US/Alaska') #combine date and time into one column
swim$end.datetime <- as.POSIXct(paste(swim$Arr.Land, swim$Time.Arr), tz='US/Alaska')
all$datetime <- as.POSIXct(all$datetime, tz='US/Alaska')

#create id columns
swim$id <- paste(swim$Animal, swim$Year, sep='.')
all$id <- paste(all$animal, all$year, sep='.')

#add 1's to locations included in swims
all$swim <- 0 
id <- swim$id
for (i in 1:length(id)){
  all$swim[all$datetime <= swim$end.datetime[i] & all$datetime >= swim$start.datetime[i] & all$id==id[i]] <- 1
}


test.swim <- subset(all, swim==1) ## test - it works!

save(all, file='all.RData')

###########################################

# Make sure start.swim and end.swim reflect new swims #

# resent all previous start. and end.swims to 0
all$start.swim <- 0
all$end.swim <- 0

## create groups for start and end of swims based on swim = 1 
first =  test.swim %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice(1) 

first$start.swim <- 1

last = test.swim %>%
  group_by(id) %>%
  arrange(id,datetime) %>%
  slice(n())

last$end.swim <- 1

### merge new start.swim and end.swim into all.Rdata ####

all$id.datetime <- paste(all$id, all$datetime)

first$tt <- paste(first$id, first$datetime) #temp start
last$tt <- paste(last$id, last$datetime) #temp end
tt.start <- first$tt
tt.end <- last$tt
all$start.swim <- ifelse(all$id.datetime %in% tt.start, 1,0)
all$end.swim <- ifelse(all$id.datetime %in% tt.end, 1,0)








