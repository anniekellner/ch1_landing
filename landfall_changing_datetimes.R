all <- read.csv("ows_land_v2.csv")
sub <- Summary.traj.pb

head(all$datetime)
class(sub$date.begin)

all$datetime <- as.POSIXct(all$datetime, tz="US/Alaska")  

head(sub)
sub2 <- select(sub, id, date.begin)
class(sub2$date.begin)
sub2$date.begin <- as.character(sub2$date.begin)
class(sub2$date.begin)
head(sub2)



as.POSIXct(sub2$date.begin, format="%Y-%m-%d %H:%M:%S") #nope

strptime(sub2$date.begin, "%m/%d/%Y %H:%M:%S")

 

as.POSIXct(sub2$date.begin)


library(lubridate)
mdy_hms(sub2$date.begin, sep="-", tz="US/Alaska")

ymd_hms(all$datetime, sep="-", tz=)


condition <- sub2[1,]
row <- subset(all, animal=="pb_06810" & datetime == "2008")

class(condition[,2])
class(all$datetime)

head(all)
all <- read.csv("ows_land_v2.csv")

as.POSIXct(all$datetime)
as.POSIXct(sub2$datetime)
