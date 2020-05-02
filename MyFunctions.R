####################################
#####     FUNCTIONS   ##############
####################################

## Functions used in Dissertation Chapter 1 ########

DFtoSF <- function(df, projection) {
  df <- droplevels(df)
  sf.wgs <- st_as_sf(df, coords = c('gps_lon', 'gps_lat'), crs = 4326, dim = 'XY')
  sf.project <- st_transform(sf.wgs, projection)
  return(sf.project)
}
