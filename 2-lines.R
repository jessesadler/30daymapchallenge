## 2. Lines: Roman roads ##

library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(rnaturalearth)

shoreline <- st_read("data/awmc/Physical Data/shoreline/shoreline.geojson")
roads <- st_read("data/awmc/Cultural-Data/roads/roads.geojson")

shoreline_proj <- st_transform(shoreline, crs = st_crs(3035))
roads_proj <- st_transform(roads, crs = st_crs(3035))

major_roads <- filter(roads_proj, Major_or_M == 1)

ggplot() + 
  geom_sf(data = shoreline_proj, color = "gray20", linewidth = 0.3) + 
  geom_sf(data = major_roads, color = "darkorange4", linewidth = 0.4) + 
  coord_sf(datum = NA,
           expand = FALSE,
           xlim = c(2600000, 7000000),
           ylim = c(800000, 3600000)) + 
  labs(title = "Major roads in the Roman Empire",
       caption = paste0("Coastline and road data from Ancient World",
                        " Mapping Center: https://github.com/AWMC/geodata")) + 
  theme_bw(base_family = "Roboto Condensed",
           base_size = 13)

ggsave("plots/2-lines.png", height = 8, width = 12)

# Roads around Rome -------------------------------------------------------

roads_6707 <- st_transform(roads, crs = st_crs(6707))

ggplot() + 
  geom_sf(data = roads_6707, color = "firebrick", linewidth = 0.3) + 
  coord_sf(datum = NA,
           expand = FALSE,
           xlim = c(750000, 850000),
           ylim = c(4580000, 4680000)) + 
  labs(title = "Roads around the city of Rome during the Roman Empire",
       caption = "Data from Ancient World Mapping Center: https://github.com/AWMC/geodata") + 
  theme_bw(base_family = "Roboto Condensed",
           base_size = 13)

ggsave("plots/2-lines-rome.png", height = 8, width = 12)

