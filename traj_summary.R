library(adehabitatLT)

land <- read.csv("land_land_v2.csv")

head(land)

land$datetime <- as.POSIXct(land$datetime) #change datetime to POSIXct to be read by adehabitatLT
head(land)

traj.pb<-as.ltraj(xy=land[,c("X","Y")], date=land$datetime, id=as.character(land$animal))
Summary.traj.pb <- summary(traj.pb)

Summary.traj.pb$DaysTrack <-round(difftime(Summary.traj.pb$date.end,Summary.traj.pb$date.begin, units="days"),digits=1)
Summary.traj.pb$Records <- Summary.traj.pb$nb.reloc-Summary.traj.pb$NAs
Summary.traj.pb$PctComplete <- round((Summary.traj.pb$nb.reloc-Summary.traj.pb$NAs)/Summary.traj.pb$nb.reloc*100,digits=1)

head(Summary.traj.pb)
write.csv(Summary.traj.pb, file = "traj_summary_v1.csv")
