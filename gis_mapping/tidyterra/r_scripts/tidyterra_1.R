# tidyterra

# https://dieghernan.github.io/tidyterra/index.html

# The goal of tidyterra is to provide common methods 
# of the tidyverse packages for objects created with the 
# terra package: SpatRaster and SpatVector. It also provides 
# geoms for plotting these objects with ggplot2.

# Install package

install.packages("tidyterra")

# Example

############ SpatRasters #############

library(tidyterra)
library(terra)

# Temperatures
rastertemp <- rast(system.file("extdata/cyl_temp.tif", package = "tidyterra"))

rastertemp

# Rename
rastertemp <- rastertemp %>%
  rename(April = tavg_04, May = tavg_05, June = tavg_06)

# Facet all layers
library(ggplot2)

ggplot() +
  geom_spatraster(data = rastertemp) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(suffix = "º"),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    fill = "",
    title = "Average temperature in Castille and Leon (Spain)",
    subtitle = "Months of April, May and June"
  )




# Create maximum differences of two months
variation <- rastertemp %>%
  mutate(diff = June - May) %>%
  select(variation = diff)

# Add also a overlay of a SpatVector
prov <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

ggplot(prov) +
  geom_spatraster(data = variation) +
  geom_spatvector(fill = NA) +
  scale_fill_whitebox_c(
    palette = "deep", direction = -1,
    labels = scales::label_number(suffix = "º"),
    n.breaks = 5
  ) +
  theme_minimal() +
  coord_sf(crs = 25830) +
  labs(
    fill = "Variation",
    title = "Variation of Temperature in Castile and León (Spain)",
    subtitle = "Average Temperatures: June vs. May"
  )




rgb_tile <- rast(system.file("extdata/cyl_tile.tif", package = "tidyterra"))

plot <- ggplot(prov) +
  geom_spatraster_rgb(data = rgb_tile) +
  geom_spatvector(fill = NA) +
  theme_light()

plot




# Automatically recognizes and applies coord_sf() for spatial data.
plot +
  # Change the CRS and datum (useful for relabeling graticules).
  coord_sf(crs = 3857, datum = 3857)



asia <- rast(system.file("extdata/asia.tif", package = "tidyterra"))

terra::plot(asia)




ggplot() +
  geom_spatraster(data = asia) +
  scale_fill_hypso_tint_c(
    palette = "gmt_globe",
    labels = scales::label_number(),
    # Further refinements
    breaks = c(-10000, -5000, 0, 2000, 5000, 8000),
    guide = guide_colorbar(reverse = TRUE)
  ) +
  labs(
    fill = "elevation (m)",
    title = "Hypsometric map of Asia"
  ) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.width = rel(3),
    legend.ticks = element_line(colour = "black", linewidth = 0.3),
    legend.direction = "horizontal"
  )




########### SpatVectors ################




vect(system.file("ex/lux.shp", package = "terra")) %>%
  mutate(pop_dens = POP / AREA) %>%
  glimpse() %>%
  autoplot(aes(fill = pop_dens)) +
  scale_fill_whitebox_c(palette = "pi_y_g") +
  labs(
    fill = "population per km2",
    title = "Population density of Luxembourg",
    subtitle = "By canton"
  )






