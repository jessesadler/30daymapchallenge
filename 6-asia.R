## 6. Asia: Places in Pleiades data set ##

library(sf)
library(readr)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(ggtext)
library(rnaturalearth)

sf_use_s2(FALSE)

# Data --------------------------------------------------------------------

# Natural Earth data
land <- ne_download(scale = 10,
                    type = "land",
                    category = "physical",
                    returnclass = "sf")
countries <- ne_download(scale = 10,
                         type = "countries",
                         category = "cultural",
                         returnclass = "sf")
rivers <- ne_download(scale = 10,
                      type = "rivers_lake_centerlines",
                      category = "physical",
                      returnclass = "sf")
lakes <- ne_download(scale = 10,
                     type = "lakes",
                     category = "physical",
                     returnclass = "sf")

# Major settlements as determined by DARE data
major_settlements <- read_csv("data/dare-major-settlements.csv")
# Pleiades data
points <- read_csv("data/pleiades/location_points.csv") |> 
  select(place_id, year_after_which, year_before_which, geometry_wkt)
names <- read_csv("data/pleiades/names.csv") |> 
  select(place_id, title)

# AWMC data: Empire of Alexander the Great
alexander_poly <- 
  st_read("data/awmc/Cultural-Data/political_shading/alexanders_empire/alexanders_empire.geojson")



# Settlements
settlements <- major_settlements |> 
  left_join(points, by = "place_id", multiple = "first") |> 
  filter(!is.na(geometry_wkt)) |> 
  st_as_sf(wkt = "geometry_wkt", crs = 4326)

# Alexander settlements
alexander <- points |> 
  left_join(names, by = "place_id", multiple = "all") |>
  filter(str_detect(title, "Alexand")) |> 
  distinct(place_id, .keep_all = TRUE) |> 
  st_as_sf(wkt = "geometry_wkt", crs = 4326)

# Asia to subset data -----------------------------------------------------
asia <- countries |> 
  filter(CONTINENT == "Asia") |> 
  group_by("CONTINENT") |> 
  summarise(do_union = FALSE)

# Clip points
asia_points <- st_intersection(settlements, st_geometry(asia))
asia_alexander <- st_intersection(alexander, st_geometry(asia))
alexander_poly <- st_intersection(alexander_poly, st_geometry(asia))

# Remove points in European Turkey west of Bosporus
turkey <- countries |> 
  select(SOVEREIGNT) |> 
  filter(SOVEREIGNT == "Turkey") |> 
  st_cast("POLYGON") |> 
  tibble::rownames_to_column("id")

euro_turkey <- turkey |> 
  filter(id %in% c(2, 4)) |> 
  group_by(SOVEREIGNT) |> 
  summarise() |> 
  st_geometry()

# Clip points
asia_points <- st_difference(asia_points, euro_turkey)
asia_alexander <- st_difference(asia_alexander, euro_turkey)
alexander_poly <- st_difference(alexander_poly, euro_turkey)

st_bbox(asia_points)

styled_text <- 
  "With places named after <span style='color:#F8766D;'>**Alexander the Great**</span> highlighted"

ggplot() + 
  geom_sf(data = land) + 
  geom_sf(data = alexander_poly[1, ], linetype = 3, fill = "gray80") + 
  geom_sf(data = filter(rivers, scalerank < 8), color = "white", linewidth = 0.3) + 
  geom_sf(data = lakes, fill = "white") + 
  geom_sf(data = asia_points, alpha = 0.7, size = 1.5) + 
  geom_sf(data = asia_alexander, color = "#F8766D", size = 1.5) + 
  annotate(text, x = 61.5, y = 27,
           label = "Empire of Alexander the Great in 323 BCE")
  coord_sf(datum = NA,
           expand = TRUE,
           xlim = c(24, 74),
           ylim = c(22, 46)) + 
  labs(title = "Ancient Settlements of the Roman World in Asia",
       subtitle = styled_text,
       caption = "Data from Pleiades: https://pleiades.stoa.org") + 
  theme_bw(base_family = "Roboto Condensed",
           base_size = 13) + 
  theme(plot.subtitle = element_markdown())

ggsave("plots/6-asia.png", height = 8, width = 12)
