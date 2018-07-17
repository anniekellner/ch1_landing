library(adehabitatLT)

land <- read.csv("ows_land_v2.csv")

head(land)

land$datetime <- as.POSIXct(land$datetime, tz = "US/Alaska") #change datetime to POSIXct to be read by adehabitatLT
head(land)

traj.pb<-as.ltraj(xy=land[,c("X","Y")], date=land$datetime, id=as.character(land$animal))
Summary.traj.pb <- summary(traj.pb)

Summary.traj.pb$DaysTrack <-round(difftime(Summary.traj.pb$date.end,Summary.traj.pb$date.begin, units="days"),digits=1)
Summary.traj.pb$Records <- Summary.traj.pb$nb.reloc-Summary.traj.pb$NAs
Summary.traj.pb$PctComplete <- round((Summary.traj.pb$nb.reloc-Summary.traj.pb$NAs)/Summary.traj.pb$nb.reloc*100,digits=1)

head(Summary.traj.pb)
write.csv(Summary.traj.pb, file = "traj_summary_v1.csv")


###### Creating landfall dataframe ######

id<-unique(traj.pb$id)
out<-data.frame()
for (i in 1:length(id)) {
  sub<-traj.pb[traj.pb$id==id[i],]
  first<-sub[1,]
  out<-rbind(out, first)
}
dim(out)



########
## Playing around with dates and times ##
########

traj.pb <- ld(traj.pb)
head(traj.pb)
class(traj.pb$date)

traj.pb[1,3]

head(Summary.traj.pb)
Sum <- Summary.traj.pb
Sum[1,5]

class(Sum)
head(Sum)
 

