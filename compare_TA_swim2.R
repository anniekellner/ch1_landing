test <- subset(all, animal=='pb_06810')
head(test)
land <- subset(all, land==1 & ows==1)
pb <- subset(land, animal=='pb_20413')
head(pb)

pb <- subset(all, animal=='pb_21361' & year==2013 & land==1)
head(pb)

unique()

strsplit(ta.gps$ï..id, ".")




