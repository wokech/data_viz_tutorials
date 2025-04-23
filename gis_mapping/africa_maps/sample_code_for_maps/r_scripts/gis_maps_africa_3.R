library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Load the Natural Earth data
africa <- ne_countries(continent = "Africa", returnclass = "sf")

# Plot the map using ggplot2
ggplot(data = africa) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Map of Africa")
