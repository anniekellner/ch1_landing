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


head(gps)

gps@data[gps$animal==pb_20492]
class(gps$animal)
gps$animal <- as.character(gps$animal)

gps@data[gps$animal=="pb_20492",] #testing whether selection works
gps[gps$animal=="pb_20492",] #not sure what the difference is between these...one has spatial data AND attribute data?


condition <- 



output= matrix(nrow = 38, ncol = 16){
  for(i in 1:nrows{
    
  })
}




library(dplyr)


lf <- filter(gps, animal==c("pb_06810", "pb_06817", "pb_20176", "pb_20333", "pb_20413",
                            "pb_20414", "pb_20418",
                            "pb_20446",
                            "pb_20492",
                            "pb_20520",
                            "pb_20525",
                            "pb_20529",
                            "pb_20586",
                            "pb_20598",
                            "pb_20735",
                            "pb_20741",
                            "pb_20809",
                            "pb_20845",
                            "pb_20965",
                            "pb_20966",
                            "pb_20974",
                            "pb_20975",
                            "pb_20982",
                            "pb_21015",
                            "pb_21219",
                            "pb_21237",
                            "pb_21238",
                            "pb_21264",
                            "pb_21307",
                            "pb_21343",
                            "pb_21358",
                            "pb_21368",
                            "pb_21380",
                            "pb_32255",
                            "pb_32282",
                            "pb_32366",
                            "pb_32608",
                            "pb_32698") & datetime == c("8/29/2008 3:00",
                                                      "9/22/2006 0:01",
                                                      "9/29/2005 16:00",
                                                      "6/15/2008 8:00",
                                                      "8/24/2006 16:00",
                                                      "7/23/2009 4:00",
                                                      "8/24/2005 16:00",
                                                      "7/31/2009 11:00",
                                                      "8/16/2008 3:00",
                                                      "7/4/2012 2:00",
                                                      "8/18/2013 20:01",
                                                      "7/8/2004 16:00",
                                                      "8/23/2008 3:00",
                                                      "11/5/2006 2:00",
                                                      "7/27/2009 4:00",
                                                      "8/24/2008 0:00",
                                                      "11/2/2005 16:00",
                                                      "10/9/2009 20:30",
                                                      "8/24/2008 22:00",
                                                      "8/11/2008 0:00",
                                                      "8/19/2008 21:00",
                                                      "8/22/2008 22:00",
                                                      "8/28/2008 20:00",
                                                      "8/8/2013 6:02",
                                                      "11/10/2011 20:00",
                                                      "9/10/2011 4:00",
                                                      "6/26/2011 0:00",
                                                      "6/22/2011 4:00",
                                                      "6/21/2012 4:00",
                                                      "6/28/2015 22:00",
                                                      "8/17/2013 20:00",
                                                      "8/11/2014 22:00",
                                                      "7/30/2015 3:00",
                                                      "8/26/2008 23:00",
                                                      "8/25/2008 23:00",
                                                      "8/10/2011 4:00",
                                                      "8/20/2008 23:00",
                                                      "6/16/2015 4:00" 
                            ))
  
 
