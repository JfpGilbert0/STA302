library(ggplot2)
library(janitor)
library(tidyverse)
library(leaflet)

location_data <- data.frame(
  latitude = c(37.7749, 34.0522, 41.8781),  # Example latitudes
  longitude = c(-122.4194, -118.2437, -87.6298)  # Example longitudes
)

# Create a leaflet map and add markers
map <-
  leaflet(location_data) %>%
  addTiles() %>%
  addMarkers(
    lng = ~longitude,
    lat = ~latitude,
    label = ~paste("Latitude:", latitude, "<br>Longitude:", longitude)
  )
print(map)

ggplot(data = penguins,
       aes(x = flipper_length_mm,
           y = body_mass_g)) +
  geom_point(aes(color = species,
                 shape = species),
             size = 20,
             alpha = 0.8) +
  scale_color_manual(values = c("grey", "darkgrey", "lightgrey")) +
  labs(
    title = "Penguin size, Palmer Station LTER",
    subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Penguin species",
    shape = "Penguin species"
  ) +
  theme_dark() +
  theme(
    legend.position = c(0.2, 0.7),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, face = "italic"),
    plot.caption.position = "plot"
  )

