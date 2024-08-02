# # Install and load required packages
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("rnaturalearth")
# install.packages("rworldmap")

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rworldmap)

# Load Natural Earth data for Africa countries
world <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Simulated GDP per capita data (replace with actual data)
set.seed(123)
world$gdp_per_capita <- runif(n = nrow(world), min = 100, max = 20000)

# Plotting GDP per capita for African countries
ggplot() +
  geom_sf(data = world, aes(fill = gdp_per_capita)) +
  scale_fill_viridis_c(name = "GDP per capita") +
  theme_minimal()