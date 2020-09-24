#########################################################################
####      ADD BEARS FOR COX PH REGRESSION ANALYSIS    ###################
#########################################################################

# Add pb_20333.2008 per Pagano et al. 2012
# Add pb_20525.2014 per ice_arrive_depart.csv
# I called both bears "undecided" but am now including

load("all_v2.RData")

lb <- subset(all.v2, land_bear == 1)
ss <- subset(lb, start.swim == 1) # 16 swims

# Add departure date

all.v2$ymd <- as.character(all.v2$ymd)
all.v2$id.ymd <- paste(all.v2$id, all.v2$ymd)

which(all.v2$id.ymd == "pb_20333.2008 2008-08-15") # scrolled through this date to find entry that matches Pagano et al. 2012
which(all.v2$id.datetime == "pb_20525.2014 2014-08-14 16:00:29")
all.v2[11495,]$start.swim <- 1
all.v2[53228,]$start.swim <- 1

save(all.v2, file = "all_v2.RData")

# Add ice information