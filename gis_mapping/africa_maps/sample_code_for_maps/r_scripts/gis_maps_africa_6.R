# Regional Mapping 

# Using ggplot2 and sf
# Use WGS84 or crs = 4326 for the World Geodetic System which is 
# utilized in GPS

# 1. EAC

# Load necessary libraries
library(ggplot2)
library(sf)
library(rnaturalearth)

# Define the EAC countries
eac_countries <- c("Burundi", "Kenya", "Rwanda", "S. Sudan", "Tanzania", "Uganda", "Somalia", "Dem. Rep. Congo")

# Load world map data
world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Filter the world map data to include only the EAC countries
eac_map <- world_map %>%
  filter(name %in% eac_countries) %>%
  st_transform(crs = 4326)  # Reproject to WGS84

# Create the map
ggplot() +
  geom_sf(data = eac_map, color = "black", fill = "lightgreen", size = 0.2) +
  labs(
    title = "EAC Countries", x = "", y = "") +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) 

# 2. ECOWAS

# Load necessary libraries
library(ggplot2)
library(rnaturalearth)
library(sf)

# Define the ECOWAS countries
ecowas_countries <- c("Benin", "Burkina Faso", "Cabo Verde", "Côte d'Ivoire", "Gambia", "Ghana",
                      "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal",
                      "Sierra Leone", "Togo")

# Load world map data
world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Filter the world map data to include only the ECOWAS countries
ecowas_map <- world_map %>%
  filter(name %in% ecowas_countries) %>%
  st_transform(crs = 4326)  # Reproject to WGS84

# Create the map
ggplot() +
  geom_sf(data = ecowas_map, fill = "lightblue", color = "black", size = 0.2) +
  labs(title = "ECOWAS Countries", x = "", y = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# 3. SADC

# Load necessary libraries
library(ggplot2)
library(rnaturalearth)
library(sf)

# Define the SADC countries
sadc_countries <- c("Angola", "Botswana", "Comoros", "Democratic Republic of the Congo",
                    "eSwatini", "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique",
                    "Namibia", "Seychelles", "South Africa", "Tanzania", "Zambia", "Zimbabwe")

# Load world map data
world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Filter the world map data to include only the SADC countries
sadc_map <- world_map %>%
  filter(name %in% sadc_countries) %>%
  st_transform(crs = 4326)  # Reproject to WGS84

# Create the map
ggplot() +
  geom_sf(data = sadc_map, fill = "lightgreen", color = "black", size = 0.2) +
  labs(title = "SADC Countries", x = "", y = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


