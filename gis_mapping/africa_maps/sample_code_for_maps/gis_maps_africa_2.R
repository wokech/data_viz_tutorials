
# library(ggmap)
# # Create a map
# map <- ggmap(get_map(location = c(lon = 0, lat = 0), zoom = 4))
# # Add a layer
# map + geom_point(aes(x = 0, y = 0), color = "red")

library(leaflet)
# Create a map
leaflet() %>% 
  setView(lng = 0, lat = 0, zoom = 4) %>% 
  addTiles() %>% 
  addMarkers(lng = 0, lat = 0, popup = "Marker")

# library(tmap)
# # Create a map
# tm_map("world", "Africa") %>% 
#   tm_fill("HDI", palette = "YlGnBu")

library(cartography)
# Create a map
cartography::carto_map("Africa", "HDI", palette = "YlGnBu")