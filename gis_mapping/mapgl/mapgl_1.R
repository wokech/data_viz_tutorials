# Getting started with mapgl
# https://walker-data.com/mapgl/articles/getting-started.html

# Using Mapbox GL JS
# Before it gets to CRAN install via github
remotes::install_github("walkerke/mapgl")
library(mapgl)
library(tidyverse)

# Using Mapbox GL JS
# Style 1
mapboxgl()

# Style 2

mapboxgl(
  style = mapbox_style("satellite"),
  projection = "winkelTripel")

# Style 3

mapboxgl(
  center = c(-97.6, 25.4)
) |> 
  fly_to(
    center = c(-96.810481, 32.790869),
    zoom = 18.4,
    pitch = 75,
    bearing = 136.8
  )

# Try Maplibre GL JS

#Style 1
maplibre()

#Style 2
maplibre(
  style = maptiler_style("bright"),
  center = c(-43.23412, -22.91370),
  zoom = 14
) |> 
  add_fullscreen_control(position = "top-left") |> 
  add_navigation_control()

# Style 3

m1 <- mapboxgl()
m2 <- mapboxgl(mapbox_style("satellite-streets"))

compare(m1, m2)

# Kyle Walker LinkedIn post

mapboxgl(zoom = 16.77,
         center = c(2.35035, 48.85306),
         pitch = 75,
         bearing = 95.2) %>%
  set_config_property(
    "basemap",
    "lightPreset",
    "night" # Swap in "dawn", "day", or "dusk"
  )
