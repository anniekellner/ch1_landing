#'----
#'title: Raster extraction
#'author: Nathan Hahn
#'----


#' Extract used covariates and assign season. Rasters calculated in qgis and raster calcs.r  


# Set environment to EAT
Sys.setenv(TZ="Africa/Nairobi") 

library(dplyr)
library(sp)
library(raster)
library(data.table)
library(forcats)


#' ---- Prepare data for rasters ----


# load RData
df <- readRDS('all_ele.rds')


# summarize relocs by individual  
library(dplyr)
temp <- df %>%
  group_by(id) %>%
  tally()



# remove rows with NA
df <- df[complete.cases(df[,1:2]),]


#' ---- Import and prep raster data ----


# 1=farm, 2=forest, 3=water

library(raster)
# system CRS
study.area <- '+proj=utm +init=epsg:32737'

# get rasters from MEP spatial data folder and set CRS
slope <- raster(file.choose())

dist2ag <- raster(file.choose())

dist2forest <- raster(file.choose())

dist2water <- raster(file.choose())

pct.ag <- raster(file.choose())

pct.forest <- raster(file.choose())

pa <- raster(file.choose())


#' ---- Attach Raster Covariates ----
# extract raster values at each relocation point
 
 
library(sp)
# create a library of 
used <- matrix(1, nrow = nrow(df), ncol = 8)
# create spatial points 
locs <- SpatialPointsDataFrame(as.matrix(df[c("x","y")]), data = df, 
                               proj4string = crs(study.area))

used[,2] <- extract(dist2ag, locs)
used[,3] <- extract(dist2forest, locs)
used[,4] <- extract(dist2water, locs)
used[,5] <- extract(slope, locs)
used[,6] <- round(extract(pct.ag, locs), 4)
used[,7] <- round(extract(pct.forest, locs), 4)
used[,8] <- extract(pa, locs)
# add mean NDVI?

# check
head(used)
summary(used)

# create data frame - adjust to ST or AWT as needed
mode(used) = "numeric"
used <- as.data.frame(used)
used$ID <- as.character(locs@data$id)
colnames(used) <- c("used", "dist2ag", "dist2forest", "dist2water", "slope", "pct.ag", "pct.forest", "pa", "merge_id")
head(used)

library(dplyr)
used.df <- cbind(df, used)
head(used.df)

test <- subset(used.df, id != merge_id)
nrow(test) # should be zero

# select relevant cols for extraction 
used.df <- used.df %>%
  dplyr::select(x, y, date, dist, name, id, burst, collar, max.clust, 
                pa, dist2ag, dist2forest, dist2water, slope, pct.ag, pct.forest) %>%
  dplyr::mutate(pa = as.factor(pa), max.clust = as.factor(max.clust))

# standardize covariate measurements - check column numbers! 
dfStand <- as.data.frame(scale(used.df[11:16]))

used.df.standard <- cbind(used.df[1:10], dfStand)

# check summary. Means of covariates should be zero
head(used.df.standard)
summary(used.df.standard)


#' ---- Add hour of day (hour) ----
 

library(lubridate)
used.df.standard$hour <- hour(used.df.standard$date)

#' Day = 1
#' Night = 2
used.df.standard$ToD <- as.factor(ifelse(used.df.standard$hour <= 6 | used.df.standard$hour > 18, 2, 1))
head(used.df.standard)


#' ---- Add season variable ----
#' Assign season (wet or dry) based on season window dates indentified by seasonal NDVI mixture model. Season is added as a column to the main ele mov dataframe with covariates
#' Wet = 1
#' Dry = 2 


# combined seasons in single csv
windows <- read.csv("C:/Users/nhahn/Dropbox (Personal)/CSU/MEP/Data/spatial data/dry_season_mov_window_dates.csv")
windows$start <- as.POSIXct(strptime(windows$start, format = "%Y-%m-%d %H:%M:%S", tz="Africa/Nairobi" ))
windows$end <- as.POSIXct(strptime(windows$end, format = "%Y-%m-%d %H:%M:%S", tz="Africa/Nairobi" ))

df <- used.df.standard # for code simplicity

# convert dfs to data.tables
setDT(df)[, date2 := date] # duplicate date. foverlaps requires a date range in both tables
setDT(windows) 

# set key for windows
setkey(windows, start, end)

# merge tables by date range, using type "within"
tbl <- data.table::foverlaps(df, windows, by.x=c("date", "date2"), type = "within", nomatch = "NA")

# clean df of extra columns and assign season values
library(forcats)
df2 <- tbl %>%
  setDF() %>%
  mutate(season = as.factor(ifelse(is.na(start), 1, 2))) %>%
  dplyr::select(-c(start, end, date2)) %>%
  mutate(pa = recode_factor(pa,
                            "1" = "2",
                            "2" = "1"))
# pa levels with no protection, group ranch, cc, NR
df2$pa <- factor(df2$pa, levels = c("0", "1", "2", "3"))


used.df.standard <- df2
summary(used.df.standard)

##############################
## STOP - CHECK DATA - STOP ##

head(used.df.standard)
summary(used.df.standard)
str(used.df.standard)

##############################

#' ---- Save Files ----

## STOP - check file name before saving! 

# write to rds file. Name based on st or AWT
saveRDS(used.df.standard, file = "used_all_standard.rds")

# write to csv. name based on st or AWT
# write.csv(used.df.standard, file = "used_st.csv")

