#Code for changing factor date (01/01/2001) into three columns POSIXct

library(lubridate)
ice$date <- as.Date(ice$date, '%m/%d/%Y') # change factor to Date
ice$date <- ymd(ice$date, tz='US/Alaska')

library(tidyr)
separate(ice, date, into=c("Year", "Month", "Day"), sep='-') #separate into 3 columns for y,m,d