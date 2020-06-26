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
  aicc$deltaAIC <- aicc$AIC - min(aicc$AIC) 
  aicc$weight <- exp(-1/2*(aicc$deltaAIC))/sum(exp(-1/2*(aicc$deltaAIC)))
  aicc$weight.pct <- aicc$weight*100
  return(aicc)
}