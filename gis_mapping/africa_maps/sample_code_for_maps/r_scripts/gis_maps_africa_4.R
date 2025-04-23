library(ggmap)
library(leaflet)
library(tidyverse)

# ggmap - Google now requires an API key

# Create a map
map <- ggmap(get_map(location = c(lon = 0, lat = 0), zoom = 4))
# Add a layer
map + geom_point(aes(x = 0, y = 0), color = "red")

# leaflet - not working

# Create a map

# This is the format:
# map <- leaflet() %>%addTiles() %>% addMarkers( lng, lat, popup)

leaflet() %>% 
  setView(lng = 0, lat = 0, zoom_start = 4) %>% 
  addTiles() %>% 
  addMarkers(lng = 0, lat = 0, popup = "Marker")

# tmap 

library(tmap)
# Create a map
data(World, metro)
Africa <- World %>%
  subset(continent == 'Africa')
tm_shape(Africa) +
  tm_fill("grey70")

# cartography - review again!!!

library(cartography)
# Create a map
cartography::carto_map("Africa", "HDI", palette = "YlGnBu")

