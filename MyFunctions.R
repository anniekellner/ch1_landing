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

#' Calculate wind direction (degrees)
#' https://stackoverflow.com/questions/8673137/calculating-wind-direction-from-u-and-v-components-of-the-wind-using-lapply-or-i
#' 
#' @param u U vector
#' @param v V Vector

WindDir <- function(u, v){
  r2d = 180/pi
  winddir = atan2(v,u) * r2d
  
  if (u > 0 & v > 0) {
      Dir = 90 - winddir
  } else if (u > 0 & v < 0) {
      Dir = 90 + (winddir*-1)
  } else if (u < 0 & v > 0) {
      Dir = 360-winddir
  } else if (u < 0 & v < 0) {
      Dir = 90 + (winddir*-1)
  } else if(u == 0 & v <0) {
      Dir = 180
  } else if(u == 0 & v > 0) {
      Dir = 0
  } else if(v == 0 & u < 0) {
      Dir == 270
  } else if (v == 0 & u > 0) {
      Dir == 90
  } else if (u != 0 | v != 0) {
    Dir = winddir}
  return(Dir)
}




