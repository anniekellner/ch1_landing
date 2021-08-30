rm(list = ls())

load('all_v2.RData')
new <- read.table(file='C:/Users/akell/OneDrive - Colostate/PhD/Polar_Bears/Chapter1/Analyses/Old/Migration/Data/arrival_land_committed.csv', header = TRUE)
library(dplyr)

# Prep new.arrive for merging
new$Date  <- strptime(as.character(new$Date), "%m/%d/%Y")

library(lubridate)
new$datetime <- as.character(paste(new$Date, new$Time))
new$datetime <- ymd_hms(new$datetime)
new$tt <- paste(new$Animal, new$datetime)
tt2 <- new$tt

# merge
all.v2$animal.datetime <- paste(all$animal, all$datetime)
all$new.arrival <- ifelse(all$animal.datetime %in% tt2, 1, 0)  

# test
test <- subset(all, new.arrival==1) # make sure merge worked

test2 <- subset(test, end.swim==1) # see how many were previously called as 'arrival' dates --> 14/21
test3 <- subset(test, end.swim==0) # group that wavers between ice and land 

##############################################################################
### Pull first arrival dates for comparison ##################################

land <- subset(all, month >6 & land==1) #subset all by OWS and land

first <- land %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  slice(1)

waver <- subset(first, id=='pb_20333.2008' | id=='pb_20413.2006' | id=='pb_20418.2005' | id=='pb_20520.2012' | id=='pb_20529.2004' | id=='pb_20544.2004' | id=='pb_32227.2013')
waver2 <- full_join(waver, test)

write.csv(waver2, file='C:/Users/akell/Desktop/Spring 2019/Research/Undecided.csv')  #Write 'undecided' group to .csv

##################################################
## Look at end.swim for comparison ###############

end.swim <- subset(all, end.swim==1) # looks like many of my calls were in between 'first contact' and 'new arrival (beginning of >=7 days)'

########################################################################
## Calculate time difference  between first contact and new arrival date


#prep data
undec <- read.csv("C:/Users/akell/Desktop/Spring 2019/Research/Undecided.csv")
undec <- na.omit(undecided) #remove NA's
undec$datetime <- ymd_hms(paste(undec$year, undec$month, undec$day, undec$hour, undec$minute, undec$second, sep = "-"), tz="US/Alaska")

#calculate time difference
diff <- undec %>%
  group_by(id) %>%
  arrange(id, datetime) %>%
  mutate(interim=difftime(datetime, lag(datetime), units="hours"))

diff$interim <- diff$interim/24 #change hours to days

save(diff, file='undecided.RData')

 