## 7. Navigation: The Silk Roads ##

library(sf)
library(opencage)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(rnaturalearth)

# Data --------------------------------------------------------------------

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

places <- 
  c("Antioch", "Damascus", "Palmyra", "Babylon", "Merv", "Bukhara",
    "Samarkand", "Kashgar", "Kuqa", "Hotan", "Turpan", "Dunhuang",
    "Chang'an", "Luoyang", "Hangzhou", "Alexandria", "Muscat", "Aden")

places_geo <- oc_forward_df(places,
                            bounds = oc_bbox(25, 10, 125, 50))

other_places <- tibble(
  placename = c("Jade Gate", "Barygaza"),
  oc_lat = c(39.833333, 21.70078), oc_lng = c(97.566667, 72.983068),
  oc_formatted = c("Yumen Pass", "Barygaza")
  )

places_geo <- bind_rows(places_geo, other_places)

# Create routes on same row and use geom_curve
routes <- tibble(
  id = 1:16,
  start = c("Antioch", "Damascus", "Palmyra", "Babylon", "Merv",    
            "Bukhara", "Samarkand", "Kashgar", "Hotan", "Dunhuang",
            "Kashgar", "Kuqa", "Turpan", "Jade Gate", "Chang'an",
            "Luoyang"),
  end = c("Palmyra", "Palmyra", "Babylon", "Merv", "Bukhara",
          "Samarkand", "Kashgar", "Hotan", "Dunhuang",
          "Jade Gate", "Kuqa", "Turpan", "Jade Gate",
          "Chang'an", "Luoyang", "Hangzhou")
  )

curvature <- c(-0.1, 0.4, -0.4, -0.1, 0, 0, -0.4, 0.4,
               0.4, 0, -0.1, -0.1, 0.1, 0.1, 0, -0.4)

routes_geo <- routes |> 
  left_join(places_geo, by = join_by(start == placename)) |> 
  rename(y = oc_lat, x = oc_lng) |> 
  left_join(places_geo, by = join_by(end == placename)) |> 
  rename(yend = oc_lat, xend = oc_lng) |> 
  add_column(curvature = curvature)



# Plot --------------------------------------------------------------------

citation <- 
  paste0("Modeled after Map 17.1 The Silk Roads in ",
         "Liu, Xinru. “Regional Study: Exchanges Within the Silk Roads World System.”",
         "<br>In *The Cambridge World History, Volume 4: A World With*",
         " *States, Empires and Networks, 1200 BCE–900 CE*. Cambridge University",
         " Press, 2015, page 459.")

ggplot() + 
  geom_sf(data = land) + 
  geom_sf(data = filter(rivers, scalerank < 7), color = "white", linewidth = 0.3) + 
  geom_sf(data = lakes, fill = "white", color = NA) + 
  geom_point(data = places_geo, aes(x = oc_lng, y = oc_lat)) + 
  geom_curve(data = filter(routes_geo, curvature == -0.1),
             aes(x = x, y = y, xend = xend, yend = yend),
             curvature = -0.1) + 
  geom_curve(data = filter(routes_geo, curvature == 0.1),
             aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0.1) + 
  geom_curve(data = filter(routes_geo, curvature == -0.4),
             aes(x = x, y = y, xend = xend, yend = yend),
             curvature = -0.4) + 
  geom_curve(data = filter(routes_geo, curvature == 0.4),
             aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0.4) + 
  geom_curve(data = filter(routes_geo, curvature == 0),
             aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0) + 
  geom_text_repel(data = places_geo, seed = 42,
                  aes(x = oc_lng, y = oc_lat, label = placename)) + 
  coord_sf(datum = NA,
           expand = FALSE,
           xlim = c(24, 124),
           ylim = c(5, 46)) + 
  labs(title = "The Ancient Silk Roads",
       caption = citation,
       x = NULL, y = NULL) + 
  theme_bw(base_family = "Roboto Condensed",
           base_size = 13) + 
  theme(plot.caption = element_markdown(hjust = 0))

ggsave("plots/7-navigation.png", height = 8, width = 12)
