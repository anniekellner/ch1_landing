###########################################
####   ID AND REMOVE DUPLICATE ENTRIES    #######
###########################################

load("land_bears_CoxPH.RData")

# See whether there are duplicate id/datetimes compared to the distinct() function

test_dup_id.datetime <- bears %>% # bears with duplicate id and datetime
  select(id.datetime)

test2_dup_id.datetime <- distinct(test_dup_id.datetime) # 3542 duplicates  (27373 - 23831)

dup_bears <- distinct(bears) # 2734 duplicates (3542 - 2734 = 808 duplicates not detected by distinct())

# There are likely 808 duplicates in which multiple measurements exist for one (or more) variables

dup_bears <- dup_bears %>% # pb_20525.2014 is the culprit (1616 total entries)
  group_by(id.datetime) %>%
  filter(n() > 1)

pb20525.2014 <- bears %>% # 2806 entries from bears dataframe (distinct() not applied)
  filter(id == "pb_20525.2014") 
  # dist2land is the variable that has two values per id.datetime

# The values are very close so unlikely to make a difference in the results. Selecting first values. 

bears <- bears %>% # remove pb_20525.2014 from bears dataframe
  filter(id != "pb_20525.2014")

pb20525.2014_head <- pb20525.2014 %>% # 1403
  group_by(id.datetime) %>%
  slice_head()

bears <- rbind(pb20525.2014_head, bears)

save(bears, file = "coxph.RData")




