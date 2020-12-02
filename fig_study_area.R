#########################################################
###   MAP OF STUDY AREA   ###############################
#########################################################


library(ggOceanMaps) # loads ggplot2  # https://github.com/MikkoVihtakari/ggOceanMaps
library(sf)

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

polar <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=-152.5 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
box <- c(xmin = -176, xmax = -129, ymin = 64, ymax = 80)

geog_lines <- st_read("C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots/ne_10m_geographic_lines/ne_10m_geographic_lines.shp")
arctic_circle <- geog_lines[4,]
arctic_circle_crop <- st_crop(arctic_circle, box)


#arctic_circle <- st_transform(arctic_circle, crs = polar)
#arctic_circle <- st_crop()

borders <- st_read("C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots/cb_2018_us_nation_5m/cb_2018_us_nation_5m.shp")
usca <- st_crop(borders, box)
# CREATE BOX FOR STUDY AREA

# Create lines for Arctic Circle and US-Canada border


inset <- basemap(limits = 60, bathymetry = TRUE, land.col = "#9ECBA0") + 
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, size = 2)) + 
  geom_sf(data = mcp, color = "red", fill = NA, lwd = 1, show.legend = TRUE)  

mcp <- st_read("./Shapefiles/95ca_ows_land_bears.shp")
mcp <- st_transform(mcp, crs = 4326)


ggsave("arctic_birdseye.png", plot = inset, path = "C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots")

main <- basemap(limits = c(-176, -129, 66, 80), rotate = TRUE, bathymetry = TRUE, bathy.style = "poly_blues", land.col = "#9ECBA0") + 
  annotation_scale(location = "br") + 
  annotation_north_arrow(location = "bl", which_north = "true") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  geom_sf(data = mcp, color = "black", fill = NA) +
  geom_sf(data = arctic_circle_crop, linetype = "4A") + 
  geom_sf(data = usca, fill = NA)


ggsave("study_area.png", plot = main, path = "C:/Users/akell/Documents/PhD/Polar_Bears/R-Plots")