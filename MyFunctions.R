####################################
#####     FUNCTIONS   ##############
####################################

## Functions used in Dissertation Chapter 1 ########

#' Dataframe to SF
#' 
#' @param df Dataframe with lat/long (EPSG: 4326)
#' @param projection what I want the data projected to

DFtoSF <- function(df, projection) {
  df <- droplevels(df)
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.project <- st_transform(sf.wgs, projection)
  return(sf.project)
}

#' Create AICc table
#' create table with values AICc, deltaAIC, weight, weight.pct
#' 
#' @param aicc A dataframe comparing models with AICc results calculated using package MuMin

create_AICc_table <- function(aicc){
  aicc$Model <- rownames(aicc)
  aicc$deltaAICc <- aicc$AICc - min(aicc$AICc) 
  aicc$L <- exp(-0.5*(aicc$deltaAICc))
  aicc$weight <- aicc$L/(sum(aicc$L))
  aicc$weight.pct <- aicc$weight*100
  aicc <- arrange(aicc, aicc$deltaAICc)
  return(aicc)
}

#' Calculate wind Direction (degrees)
#' https://stackoverflow.com/questions/8673137/calculating-wind-Dir[i]ection-from-u-and-v-components-of-the-wind-using-lapply-or-i
#' 
#' @param df dataframe with wind vectors u and v


WindDir <- function(df){
  Dir <- vector()
  r2d = 180/pi
 
  for(i in 1:nrow(df)){
  windDir = atan2(df$v[i], df$u[i]) * r2d
  Dir[i] <- if (df$u[i] > 0 & df$v[i] > 0) {
      Dir = 90 - windDir
  } else if (df$u[i] > 0 & df$v[i] < 0) {
      Dir = 90 + (windDir*-1)
  } else if (df$u[i] < 0 & df$v[i] > 0) {
      Dir = 360 - windDir
  } else if (df$u[i] < 0 & df$v[i] < 0) {
      Dir = 90 + (windDir*-1)
  } else if(df$u[i] == 0 & df$v[i] < 0) {
      Dir = 180
  } else if(df$u[i] == 0 & df$v[i] > 0) {
      Dir = 0
  } else if(df$u[i] < 0 & df$v[i] == 0) {
      Dir = 270
  } else if (df$u[i] > 0 & df$v[i] == 0) {
      Dir = 90
  } else if (df$u[i] != 0 | df$v[i] != 0) {
    Dir = windDir}
  return(Dir)
  }
}


u <- 1
v <- -1

rm(list = ls())
