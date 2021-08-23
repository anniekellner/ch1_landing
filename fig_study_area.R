#########################################################
###   MAP OF STUDY AREA   ###############################
#########################################################

rm(list = ls())

library(ggOceanMaps) # loads ggplot2  # https://github.com/MikkoVihtakari/ggOceanMaps
library(sf)
library(dplyr)
library(sp)
library(raster)
library(rgdal)
library(tmap)
library(tmaptools)
#library(RColorBrewer)
library(ggplot2)

devtools::install_github("MikkoVihtakari/ggOceanMapsData") # required by ggOceanMaps
devtools::install_github("MikkoVihtakari/ggOceanMaps")

library(ggOceanMaps)
library(osmdata)

# It does not work to convert the DEM to a polygon. It is a nightmare. Breaks R. 

# To cite:

#A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ggOceanMaps: Plot Data on Oceanographic Maps using 'ggplot2'},
#>     author = {Mikko Vihtakari},
#>     year = {2020},
#>     note = {R package version 0.4.3},
#>     url = {https://mikkovihtakari.github.io/ggOceanMaps},
#>   }

# ------ LOAD DATA --------------------- #

# Projections 

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar <- CRS("+proj=stere +lat_0=90 +lat_ts=71 +lon_0=-152.5 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Include landing points

load("all_v2.RData") # to get landing points

# Landing points

end.swim <- all.v2 %>%
  dplyr::filter(end.swim == 1) %>%
  dplyr::select(animal, year, month, day, hour, minute, second, gps_lat, gps_lon, datetime, start.swim, end.swim, id, X, Y, ymd, id.datetime)

end.swim <- distinct(end.swim)
end.swim <- st_as_sf(end.swim, coords = c('X','Y'), crs = polar)

# Starting points

load('coxph.RData') #GPS data
swim <- subset(bears, start.swim == 1)
swim <- distinct(swim)


coords <- cbind(swim$X, swim$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=swim, proj4string = projection)
swim.sf <- st_as_sf(pb.spdf)

plot(st_geometry(swim.sf))

swim.sf <- st_transform(swim.sf, crs = polar)

# --- SPATIAL DATA  --------------------------- #

# Create bathymetry layer


# Crop DEM

dem <- raster('C:/Users/akell/Documents/ArcGIS/North_Slope_DEM/DEM_052520/DEM_052520/ans_dem_8bit.tif')
ext <- extent(-308644, 534700, 2212133, 2398000)
dem_crop <- crop(dem, ext)

# reclassify so just land and water

m <- c(0,26,1, 27,27,0, 28,101,1)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)

dem_rcl <- reclassify(dem_crop, rclmat) # 1 = land; 27 = sea
#writeRaster(dem_rcl, 'C:/Users/akell/Documents/ArcGIS/North_Slope_DEM/dem_ras_reclass.tif') # to view in QGIS

dem_polar <- projectRaster(dem_rcl, crs = polar) # reproject to polar stereographic

# --- tmap --------- #

bb <- getbb(place_name = 'southern Beaufort Sea')

x <- opq(bbox=bb) %>%
  add_osm_feature(key = "water")

osm <- tmaptools::read_osm(bb(q), type = 'stamen-watercolor')

palette_explorer()

tm_shape(dem_polar) + 
  tm_raster(palette = "GnBu", n = 5, legend.show = FALSE) + 
  tm_shape(swim.sf) + 
  tm_symbols(col = "red")



# Shp with 5k buffer

buf <- st_read('./data/Spatial/AK_CA_5kbuff/AK_CA_5kbuff.shp')
buf <- st_transform(buf, crs = polar)

rect <- st_read('./data/Spatial/Rectangle/rectangle.shp')
rect <- st_transform(rect, crs = polar)

# Make sure geometries align in space
plot(st_geometry(buf))
plot(rect, add = TRUE)

buf_crop <- st_crop(buf, rect)

#---- tmap-------- #
tmap_mode('plot')

palette_explorer()


tm_shape(dem_polar) + 
  tm_raster(palette = "GnBu")



# ---- Arctic Circle ------ #

box <- c(xmin = -165, xmax = -140, ymin = 66, ymax = 75)

geog_lines <- st_read("C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots/ne_10m_geographic_lines/ne_10m_geographic_lines.shp")
arctic_circle <- geog_lines[4,]
arctic_circle_crop <- st_crop(arctic_circle, box)


#arctic_circle <- st_transform(arctic_circle, crs = polar)
#arctic_circle <- st_crop()
 
borders <- st_read("C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots/cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp")
usca <- st_crop(borders, box)


inset <- basemap(limits = 60, bathymetry = TRUE, land.col = "#9ECBA0") + 
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  geom_sf(data = mcp, color = "yellow", fill = NA, lwd = 2, show.legend = TRUE) 


ggsave("arctic_birdseye.png", plot = inset, path = "C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots")

bathy <- basemap(limits = c(-165, -140, 69, 75), rotate = TRUE, bathymetry = TRUE, bathy.style = "poly_blues", land.col = "#9ECBA0") + 
    theme(legend.justification = "top") + 
    annotation_scale(location = "br") + 
    annotation_north_arrow(location = "tl", which_north = "true") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.key = element_blank()) +
    geom_raster(data = dem_polar, aes(colour = c('#52BE80', '#85C1E9')))

bathy + geom_raster(data = dem_polar)
  
  
  geom_sf(data = usca, fill = NA) +
    #geom_sf(data = arctic_circle_crop, aes(linetype = "Arctic Circle")) +
    geom_sf(data = mcp, aes(color = "95% MCP"), fill = NA, size = 2, show.legend = "polygon") + 
    geom_sf(data = swim.sf, aes(color = "Departure Points"), size = 4, shape = 18, show.legend = "point") +
    #scale_linetype_manual(values = c("Arctic Circle" = "dashed"), name = NULL, 
                          #guide = guide_legend(override.aes = list(fill=NA, shape = NA))) + 
    scale_color_manual(values = c("95% MCP" = "yellow", "Departure Points" = "#CA33FF"), name = NULL, 
                       guide = guide_legend(override.aes = list(linetype = c("blank", "blank"), fill = c(NA, NA), shape = c(22, 18), color = c("yellow", "#CA33FF")))) 


ggsave("study_area.png", plot = main, path = "C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots")

