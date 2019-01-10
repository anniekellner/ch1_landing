rm(list = ls())

load('NewDef.RData')
pb1 <- subset(x.df, animal=='pb_06817' & month==9)
pb2 <- subset(x.df, animal=='pb_20333' & month==9 | month==8)
pb3 <- subset(x.df, animal=='pb_20413' & month==9)
pb4 <- subset(x.df, animal=='pb_20414' & month==7)
pb5 <- subset(x.df, animal=='pb_20418' & year==2005 & month==9)
pb6 <- subset(x.df, animal=='pb_20446' & year==2009 & month==7)
