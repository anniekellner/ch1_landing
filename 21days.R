rm(list = ls())

gps <- readOGR("./Shapefiles/ows_land_v2.shp")
land <- readOGR("./Shapefiles/AK_CA_5kbuff.shp")

# plot land shapefile and gps points
plot(land)
points(gps)

gps2<-as.data.frame(gps)
gps2$date<-paste(gps2$year, gps2$month, gps2$day, sep="-")
gps2<-gps2[!duplicated(gps2$date),]
gps2$date<-strptime(gps2$date, format="%Y-%m-%d")

id<-unique(gps2$animal)
gps2$cont<-0
gps2$burst<-0
b<-1
out<-data.frame()
for (i in 1:length(id)) {
  sub<-gps2[gps2$animal==id[i],]
  sub<-sub[order(sub$date),]
  a<-1  
  if(nrow(sub)>2) {
    for (j in 2:nrow(sub)) {
      diff<-as.numeric(difftime(sub$date[j], sub$date[j-1]))  
      if(diff==1) { 
        sub$cont[j]<-a
        sub$burst[j]<-b
        a<-a+1
      }
      if(diff!=1) { 
        a<-1
        b<-b+1
        sub$burst[j]<-b
      }
    }
    out<-rbind(out, sub)}
}
head(out)

head(gps)