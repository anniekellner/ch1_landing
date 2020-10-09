##############################################
##    Bears with multi-year data  ############
##############################################

library(dplyr)

load("all_v2.RData")

ows <- subset(all.v2, ows == 1)

length(unique(ows$animal)) # 120 animals
length(unique(ows$id)) # 160 animals

animal <- ows %>%
  group_by(animal) %>%
  slice_head()

id <- ows %>%
  group_by(id) %>%
  slice_head()

animal.id <- unique(animal$id)
id.id <- unique(id$id)

multiyr <- setdiff(id.id, animal.id)
unique(multiyr$animal) # 40 animals

my <- strsplit(multiyr, "[.]")
my <- sapply(my, "[[", 1)

my <- unique(my)

multiyr <- subset(ows, animal %in% my)
unique(multiyr$animal) # 34 animals

first <- multiyr %>%
  group_by(id) %>%
  slice_head() %>%
  dplyr::select(animal:second, coy:land_bear)

switch <- first %>% # bears that switch between land and ice
  group_by(animal) %>%
  filter(all(c(0,1) %in% land_bear)) 
 

unique(switch$animal)
unique(switch$id)


