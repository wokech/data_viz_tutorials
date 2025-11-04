# R packages for visualising spatial data

# https://nrennie.rbind.io/blog/r-packages-for-visualising-spatial-data/

# N. Rennie

# Two dimensional maps

# Packages

library(sf)
library(tidyverse)

# Load the mapping data

uk <- sf::st_read("n_rennie_tutorials/shp_files/CTY_DEC_2022_EN_BUC.shp", quiet = TRUE)
greggs <- readr::read_csv("n_rennie_tutorials/datasets/greggs.csv")

# Convert greggs data from simple file to a spatial object

greggs_sf <- greggs %>%
  select(address.longitude, address.latitude) %>%
  rename(
    lon = address.longitude,
    lat = address.latitude
  ) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(crs = 27700)

# Base R

par(bg = "#00558e")
plot(st_geometry(uk), col = "#fab824", border = "#fab824")
plot(st_geometry(greggs_sf),
     pch = 19, col = "#00558e", cex = 0.2, add = TRUE
)

# {ggplot2}

library(ggplot2)
ggplot() +
  geom_sf(
    data = uk,
    linewidth = 0.5,
    colour = "#fab824",
    fill = "#fab824"
  ) +
  geom_sf(
    data = greggs_sf,
    size = 0.1,
    colour = "#00558e"
  ) +
  theme_void() +
  theme(plot.background = element_rect(
    fill = "#00558e",
    colour = "#00558e"
  ))


# {tmap}

library(tmap)
tm_shape(uk) +
  tm_fill(fill = "#fab824") +
  tm_borders(col = "#fab824") +
  tm_shape(greggs_sf) +
  tm_dots() +
  tm_layout(frame = FALSE, bg.color = "#00558e")


# {leaflet}

library(leaflet)
library(mapview)
new_uk <- uk %>% sf::st_transform(crs = 4326)
new_greggs <- greggs_sf %>% sf::st_transform(crs = 4326)
m <- leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = new_uk,
    stroke = FALSE,
    fillOpacity = 1,
    fillColor = "#fab824"
  ) %>%
  addCircleMarkers(
    data = new_greggs,
    radius = 0.5,
    fillOpacity = 1,
    stroke = FALSE,
    fillColor = "#00558e"
  )
m


# Three dimensional maps


library(elevatr)
library(raster)
elev_data <- get_elev_raster(
  locations = data.frame(
    x = c(-1.760, -1.335),
    y = c(54.898, 55.067)
  ),
  z = 10,
  prj = "EPSG:4326",
  clip = "locations"
)


# Base R


par(mar = c(1, 1, 3, 1), bty = 'n')
plot(elev_data, axes = FALSE, horizontal = TRUE)
title(main = "NEWCASTLE",
      adj = 0.5,
      cex.main = 1.8,
      font.main = 2,
      col.main = "black")


# {tanaka}

library(tanaka)
library(terra)
elev_raster <- rast(elev_data)
par(mar = c(1, 1, 3, 1))
tanaka(elev_raster, legend.pos = "n")
title(main = "NEWCASTLE",
      adj = 0.5,
      cex.main = 1.8,
      font.main = 2,
      col.main = "black")


# {rayshader}

library(rayshader)
elev_mat <- raster_to_matrix(elev_data)
elev_mat %>%
  sphere_shade() %>%
  plot_3d(elev_mat,
          zscale = 10, fov = 0, theta = 0, phi = 60,
          windowsize = c(600, 450),
          zoom = 0.7,
          background = "lightgrey"
  )
render_snapshot(
  filename = "rayshader.png",
  clear = FALSE,
  title_text = "NEWCASTLE",
  title_size = 50,
  title_color = "white",
  title_font = "serif"
)
