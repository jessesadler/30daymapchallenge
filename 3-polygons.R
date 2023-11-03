## 3. Polygons: Roman Empire provinces c. 200 ##

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
rivers <- ne_download(scale = 10,
                      type = "rivers_lake_centerlines",
                      category = "physical",
                      returnclass = "sf")
lakes <- ne_download(scale = 10,
                     type = "lakes",
                     category = "physical",
                     returnclass = "sf")

# Ancient World Mapping Center data
# Link to data: 
rome_200 <- 
  st_read("https://raw.githubusercontent.com/AWMC/geodata/master/Cultural-Data/political_shading/roman_empire_ce_200_extent/roman_empire_ce_200_extent.geojson")

# Lines of further division of the empire
diocletian <- 
  st_read("https://raw.githubusercontent.com/AWMC/geodata/master/Cultural-Data/political_shading/roman_empire_provinces%20post_diocletian/roman_empire_provinces%20post_diocletian.geojson")

# Transform CRS -----------------------------------------------------------
land <- st_transform(land, crs = st_crs(3035))
rivers <- st_transform(rivers, crs = st_crs(3035))
lakes <- st_transform(lakes, crs = st_crs(3035))
rome_200 <- st_transform(rome_200, crs = st_crs(3035))
diocletian <- st_transform(diocletian, crs = st_crs(3035))

# Provinces
labels <- c("Britanniae", "Galliae", "Viennensis", "Hispaniae",
             "Africa", "Oriens", "Italia", "Pannoniae",
             "Moesiae", "Thracia", "Asiana", "Pontica")


# Data wrangling ----------------------------------------------------------
# Remove Dacia
rome_200 <- filter(rome_200, OBJECTID != 119)

# Connect province name to polygon data
# Find main province polygons by area and add Britain which does not have area
provinces_poly <- filter(rome_200, Shape_Area > 10 | OBJECTID == 0)
provinces_centroid <- st_centroid(provinces_poly)

# Match name to polgon OBJECTID
labels_df <- data.frame(
  OBJECTID = c(0, 7, 18, 33, 90, 79, 14, 15, 26, 21, 50, 44),
  label = labels
)

provinces_centroid <- left_join(provinces_centroid, labels_df, by = "OBJECTID")
# Move label for Oriens to south
st_geometry(provinces_centroid)[10] <- st_geometry(provinces_centroid)[10] - c(0, 206639)

# Plot --------------------------------------------------------------------

ggplot() + 
  geom_sf(data = land, fill = "gray90") + 
  geom_sf(data = rome_200, fill = "gray85") + 
  geom_sf(data = diocletian, linetype = 3) + 
  geom_sf(data = rome_200, fill = NA, linewidth = 0.6) + 
  geom_sf(data = filter(rivers, scalerank < 7), color = "white", linewidth = 0.3) + 
  geom_sf(data = lakes, fill = "white") + 
  geom_sf_text(data = provinces_centroid, aes(label = label),
               size = 5, color = "firebrick", fontface = "bold") + 
  coord_sf(datum = NA,
           expand = FALSE,
           xlim = c(2600000, 7500000),
           ylim = c(350000, 3800000)) + 
  labs(title = "Roman dioceses under the Emperor Diocletian, r. 286–305",
       subtitle = "With outlines of the further division of the dioceses into provinces",
       caption = paste0(
         "Data from Ancient World Mapping Center: https://github.com/AWMC/geodata",
         "\nBoundaries are approximate. See http://awmc.unc.edu/wordpress/ for a similar map."),
       x = NULL, y = NULL) + 
  theme_bw(base_family = "Roboto Condensed",
           base_size = 13)

ggsave("plots/3-polygons.png", height = 8, width = 12)
