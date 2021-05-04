#########################################################
###   MAP OF STUDY AREA   ###############################
#########################################################

rm(list = ls())

library(ggOceanMaps) # loads ggplot2  # https://github.com/MikkoVihtakari/ggOceanMaps
library(sf)
library(dplyr)
library(sp)


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

load('coxph.RData') #GPS data
swim <- subset(bears, start.swim == 1)
swim <- distinct(swim)

# Projections 

projection <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") #find this in spatialreference.org
polar <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=-152.5 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"



coords <- cbind(swim$X, swim$Y)
pb.spdf <- SpatialPointsDataFrame(coords = coords, data=swim, proj4string = polar)
swim.sf <- st_as_sf(pb.spdf)

plot(st_geometry(swim.sf))

swim.sf <- st_transform(swim.sf, crs = polar)

# --- SPATIAL DATA  --------------------------- #

box <- c(xmin = -165, xmax = -140, ymin = 66, ymax = 75)

geog_lines <- st_read("C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots/ne_10m_geographic_lines/ne_10m_geographic_lines.shp")
arctic_circle <- geog_lines[4,]
arctic_circle_crop <- st_crop(arctic_circle, box)


#arctic_circle <- st_transform(arctic_circle, crs = polar)
#arctic_circle <- st_crop()

borders <- st_read("C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots/cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp")
usca <- st_crop(borders, box)
# CREATE BOX FOR STUDY AREA

# Create lines for Arctic Circle and US-Canada border

mcp <- st_read("./Shapefiles/95ca_ows_land_bears.shp")
mcp <- st_transform(mcp, crs = 4326)

inset <- basemap(limits = 60, bathymetry = TRUE, land.col = "#9ECBA0") + 
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  geom_sf(data = mcp, color = "yellow", fill = NA, lwd = 2, show.legend = TRUE) 


ggsave("arctic_birdseye.png", plot = inset, path = "C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots")

main <- basemap(limits = c(-165, -140, 66, 75), rotate = TRUE, bathymetry = TRUE, bathy.style = "poly_blues", land.col = "#9ECBA0") + 
    theme(legend.justification = "top") + 
    annotation_scale(location = "br") + 
    annotation_north_arrow(location = "tr", which_north = "true") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.key = element_blank()) +
    geom_sf(data = usca, fill = NA) +
    geom_sf(data = arctic_circle_crop, aes(linetype = "Arctic Circle")) +
    geom_sf(data = mcp, aes(color = "95% MCP"), fill = NA, size = 3, show.legend = "polygon") + 
    geom_sf(data = swim.sf, aes(color = "Departure Points"), show.legend = "point") +
    scale_linetype_manual(values = c("Arctic Circle" = "dashed"), name = NULL, 
                          guide = guide_legend(override.aes = list(fill=NA, shape = NA))) + 
    scale_color_manual(values = c("95% MCP" = "yellow", "Departure Points" = "black"), name = NULL, 
                       guide = guide_legend(override.aes = list(linetype = c("blank", "blank"), fill = c(NA, NA), shape = c(22, 16), color = c("yellow", "black")))) 


ggsave("study_area.png", plot = main, path = "C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots")

