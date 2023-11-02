## 1. Points ##

library(sf)
library(readr)
library(dplyr)
library(ggplot2)


# Data --------------------------------------------------------------------

ocean <- st_read("data/ne_ocean/ne_10m_ocean.shp")
# Major settlements as determined by DARE data
major_settlements <- read_csv("data/dare-major-settlements.csv")
# Pleiades data
points <- read_csv("data/pleiades/location_points.csv") |> 
  select(place_id, year_after_which, year_before_which, geometry_wkt)
names <- read_csv("data/pleiades/names.csv") |> 
  select(place_id, title)

settlements <- major_settlements |> 
  left_join(names, by = "place_id", multiple = "first") |>
  left_join(points, by = "place_id", multiple = "first") |> 
  filter(!is.na(geometry_wkt)) |> 
  st_as_sf(wkt = "geometry_wkt", crs = 4326) |> 
  filter(year_after_which < 0)

ggplot() + 
  geom_sf(data = ocean) + 
  geom_sf(data = settlements, alpha = 0.7, size = 1) + 
  coord_sf(datum = NA,
           expand = FALSE,
           xlim = c(-11, 43),
           ylim = c(25, 54)) + 
  labs(title = "Ancient Settlements from before the Common Era",
       caption = "Data from Pleiades: https://pleiades.stoa.org") + 
  theme_bw(base_family = "Roboto Condensed",
           base_size = 13)

ggsave("plots/1-points.png", height = 8, width = 12)


# Projected plot ----------------------------------------------------------

ocean_proj <- st_transform(ocean, crs = st_crs(3035))
settlements_proj <- st_transform(settlements, crs = st_crs(3035))

bbox <- st_bbox(settlements_proj)

ggplot() + 
  geom_sf(data = ocean_proj, fill = "white") + 
  geom_sf(data = settlements_proj, alpha = 0.7, size = 1) + 
  coord_sf(datum = NA,
           expand = FALSE,
           xlim = c(2600000, 7000000),
           ylim = c(500000, 3500000)) + 
  labs(title = "Ancient Settlements from before the Common Era",
       caption = "Data from Pleiades: https://pleiades.stoa.org") + 
  theme_bw(base_family = "Roboto Condensed",
           base_size = 13) + 
  theme(panel.background = element_rect(fill = "gray90"))

ggsave("plots/1-points-proj.png", height = 8, width = 12)
