####### SOLUTION ############ (from Guillaume)

###### Creating landfall dataframe ######

id<-unique(traj.pb$id) #from traj.pb df
out<-data.frame()
for (i in 1:length(id)) {
  sub<-traj.pb[traj.pb$id==id[i],]
  first<-sub[1,]
  out<-rbind(out, first)
}
dim(out)

## Subsetting landfall entries from ows_land_v2.csv

# setting both datetime and begin.date to POSIXct
library(dplyr)
all <- read.csv("ows_land_v2.csv")
sub <- Summary.traj.pb
sub[1,5]
sub2 <- select(sub, id, date.begin)
head(sub2)


head(all$datetime)
class(sub$date.begin)

all$datetime <- as.POSIXct(all$datetime, tz="US/Alaska")

# test loop
condition <- sub2[1,]
condition
condition[1]
condition[2]
row <- subset(all, animal %in% condition[1] & datetime %in% condition[2])

?subset
class(sub2)
class(all) #both are data.frame

class(sub2$date.begin)
class(sub2$id)
class(all$animal)
class(all$date) #object classes are aligned

#rename columns in sub2
colnames(sub2) <- c("animal", "datetime")

# test to see if something is wrong with code
head(all)
condition <- all[5, c(2,14)]
condition
all[5,14]
row <- subset(all, animal%in%condition[1] & datetime%in%condition[2])
condition[1]
condition[2]
paste(all[5,14])==paste(condition[2]," AKDT")

# Issue is likely with the datetime format - MDT v AKDT
class(all$datetime) #POSIXct
all[1,14] #"2008-10-28 05:00:00 AKDT"

class(sub$date.begin) #POSIXct
sub[1,5] #"2008-08-29 03:00:00 MDT"

class(sub2$datetime) # POSIXct
sub2[1,2] "2008-08-29 03:00:00 MDT"

#refer to traj_summary.R
class(land$datetime) #factor

class(Sum$date.begin) #POSIXct
Sum[1,5] #"2008-08-29 03:00:00 AKDT"

######## RE-DO LOOP ###########
sum2 <- select(sub, id, date.begin)
head(sum2)

# code test
condition <- sum2[1,]
condition
condition[1]
condition[2]
row <- subset(all, animal %in% condition[1] & datetime %in% condition[2])


