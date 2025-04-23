# rnaturalearth

library(rnaturalearth)
africa <- ne_countries(continent = "Africa", returnclass = "sf")
plot(world)

# Plot the map using ggplot2
ggplot(data = africa) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Map of Africa")

# gdalcubes

library(gdalcubes)
cube <- raster_cube("https://example.com/africa_data.tif")
plot(cube)

# tidycensus

library(tidycensus)
data <- get_acs(geography = "county", variables = "B01001_001", state = "KEN")
