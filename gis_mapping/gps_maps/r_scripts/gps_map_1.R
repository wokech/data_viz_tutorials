# Create custom GPS route maps in R

# https://nrennie.rbind.io/blog/gps-route-map-r/

# Loading GPS data

library(gpx)
raw_gpx <- read_gpx("gis_mapping/gps_maps/datasets/strathearn-marathon.gpx")


# Processing GPS data

library(tidyverse)
library(sf)

# We’ll then convert it to a tibble to make it more pleasant to work with, 
# before making it into a spatial data object using st_as_sf(). 

points_data <- raw_gpx$tracks$`Strathearn Marathon Complete` |>
  as_tibble() |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# We combine all of the points into one object, and then cast it to a 
# line using st_combine() and st_cast().

line_data <- points_data |>
  st_combine() |>
  st_cast("LINESTRING")


# Background map data

bbx <- st_bbox(line_data)

# Use British National Grid (BNG) coordinate reference system (27700), 
# apply the buffer, and then convert it back

line_proj <- st_transform(line_data, crs = 27700)
bbx <- st_bbox(line_proj)


# Now we can add a buffer of 500 metres so the 
# north, south, east, and west of the bounding box

buffer <- 500
bbx_expanded <- bbx
bbx_expanded[c("xmin", "ymin")] <- bbx_expanded[c("xmin", "ymin")] - buffer
bbx_expanded[c("xmax", "ymax")] <- bbx_expanded[c("xmax", "ymax")] + buffer

# Convert back

bbx_expanded <- st_bbox(st_as_sfc(bbx_expanded), crs = 27700)
bbx_expanded <- st_transform(st_as_sfc(bbx_expanded), crs = 4326)

# Use OSM data and sf

library(osmdata)
highways <- bbx_expanded |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c(
      "primary", "secondary", "tertiary", "residential",
      "living_street", "service", "unclassified",
      "pedestrian", "footway", "track", "path"
    )
  ) |>
  osmdata_sf()

roads_cropped <- st_crop(highways$osm_lines, bbx_expanded)

# Making the map

# Setting up chart variables

library(showtext)
font_add_google("Oswald")
showtext_auto()
showtext_opts(dpi = 300)
body_font <- "Oswald"


map_bg_col <- "#33658A"
map_route_col <- "grey90"
text_col <- "grey10"


library(monochromeR)
generate_palette(
  colour = map_bg_col,
  modification = "go_darker",
  n_colours = 7,
  view_palette = TRUE
)


map_line_col <- generate_palette(
  colour = map_bg_col,
  modification = "go_darker",
  n_colours = 7
)[3]


# Plotting an initial map

ggplot() +
  geom_sf(
    data = roads_cropped
  ) +
  geom_sf(
    data = line_data
  ) +
  geom_sf(data = head(points_data, 1)) +
  geom_sf(data = tail(points_data, 1))



# Styling your map


base_map <- ggplot() +
  geom_sf(
    data = roads_cropped,
    colour = map_line_col,
    linewidth = 0.5
  ) +
  geom_sf(
    data = line_data,
    colour = map_route_col,
    linewidth = 1.5
  ) +
  geom_sf(
    data = head(points_data, 1),
    colour = map_route_col,
    size = 3
  ) +
  geom_sf(
    data = tail(points_data, 1),
    colour = map_route_col,
    size = 3
  ) +
  coord_sf(expand = FALSE)
base_map


# Style the graph

library(glue)
text_map <- base_map +
  labs(
    title = str_to_upper("Strathearn Marathon"),
    subtitle = str_to_upper("12 June 2016"),
    caption = glue(
      "<span style='color: {text_col}; font-size:16pt;'>**NICOLA RENNIE**</span> #168"
    )
  )


library(ggtext)
text_map +
  theme_void(base_family = body_font, base_size = 13) +
  theme(
    panel.background = element_rect(
      fill = map_bg_col, colour = map_bg_col
    ),
    plot.background = element_rect(
      fill = map_route_col, colour = map_route_col
    ),
    plot.margin = margin(5, 20, 5, 20),
    plot.title = element_text(
      colour = text_col,
      hjust = 0,
      lineheight = 1,
      face = "bold",
      size = rel(1.6)
    ),
    plot.subtitle = element_text(
      colour = alpha(text_col, 0.7),
      hjust = 0,
      margin = margin(b = 10, t = 5),
      lineheight = 1
    ),
    plot.caption = element_textbox_simple(
      colour = alpha(text_col, 0.7),
      hjust = 1,
      halign = 1,
      margin = margin(b = 10, t = 10),
      lineheight = 1
    )
  )




