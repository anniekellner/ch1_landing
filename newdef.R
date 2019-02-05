## Redefining arrival dates based on 7-day 'dedicated stay' criteria
## A. Kellner February 2019

nuke = FALSE #do you want your environment erased
save_csv = FALSE #do you want to create a .csv

if(nuke)
rm(list = ls())


load('all.RData')
library(dplyr)
library(lubridate)
library(data.table)

#------------------------------------------------#

all$ymd <- ymd(paste(all$year, all$month, all$day)) #change date format

# count how many land points each day w/ reset by id and day
w.day = all %>%
  group_by(id, ymd) %>%
  arrange(id, datetime) %>%
  mutate(number.land = cumsum(land)) 

# create df of last rows to identify when bear has not been on land all day (1)
flag = w.day %>%
  group_by(id, ymd) %>%
  arrange(id, datetime) %>%
  slice(n()) %>%
  mutate(flag = ifelse(number.land==0,0,1))

# merge flag df with w.day df
newdef <- left_join(w.day, flag)
newdef[is.na(newdef)] <- 1
  
# create columns for time spent on land and cumulative time on land
x = newdef %>% 
  group_by(id) %>% 
  arrange(id, datetime) %>% 
  mutate(time.land=ifelse(land==0 | is.na(lag(land)) | lag(land)==0 | flag==0, 
                          0,
                          difftime(datetime, lag(datetime), units="hours"))) 
 

x.df <- as.data.frame(x) #convert to df


# create column with cumulative time on land (in days), reset when flag = 0

setDT(x.df)
x.df[, cum.land := flag*cumsum(time.land), .(id, rleid(flag))]

x.df$cum.land = x.df$cum.land/24

save(x.df, file='NewDef.RData')

test <- subset(x.df, animal=='pb_06336') #test - looks good

#------------------------------------------------------------------------------------------#

# Split dataset into two parts - 1) July through December and 2) May through November

load('NewDef.RData')
jul.dec <- subset(x.df, month >6)
land.ded <- subset(jul.dec, cum.land >=7)

first <- land.ded %>%
  group_by(id) %>%
  arrange(id,datetime) %>%
  slice(1)

if(save_csv)
write.csv(first, file='first_land_7days.csv')










