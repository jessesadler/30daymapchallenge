## 4. Bad map: Places in Pleiades data set ##

library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(rnaturalearth)

# Natural Earth data
land <- ne_download(scale = 10,
                    type = "land",
                    category = "physical",
                    returnclass = "sf")

# Points
points <- read_csv("data/pleiades/location_points.csv") |> 
  select(place_id, geometry_wkt) |> 
  st_as_sf(wkt = "geometry_wkt", crs = 4326)

bbox <- st_bbox(points)

ggplot() + 
  geom_sf(data = land) + 
  geom_sf(data = points, alpha = 0.1, size = 1) + 
  coord_sf(datum = NA,
           expand = TRUE,
           xlim = c(bbox[[1]], bbox[[3]]),
           ylim = c(bbox[[2]], bbox[[4]])) + 
  labs(title = "All 26,259 points in the Pleiades ancient places data set",
       subtitle = paste0("Just a bit of overplotting, no data cleaning, and points in South Africa",
                         " and Malay Peninsula greatly expand the bounding box of the map."),
       caption = "Data from Pleiades: https://pleiades.stoa.org") + 
  theme_bw(base_family = "Roboto Condensed",
           base_size = 13)

ggsave("plots/4-bad.png", height = 8, width = 12)
