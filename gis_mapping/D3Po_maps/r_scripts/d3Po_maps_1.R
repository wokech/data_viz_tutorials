# Creating a London Population Map with D3po
# https://pacha.dev/blog/2025/11/14/london-population/

# Mauricio “Pachá” Vargas S.

# All the po_*() functions are part of the D3po package, including po_tooltip()

# Load these R packages to import and manipulate the data:

library(d3po)
library(dplyr)
library(sf)
library(rvest)
library(janitor)

# There is a better resolution map of London boroughs provided by TfL. 
# This map looks better compared to D3po provideds subnational map (low resolution).

# Download the GeoJSON file to show that D3po can work with any spatial data in sf format.

url <- "https://hub.arcgis.com/api/v3/datasets/0a92a355a8094e0eb20a7a66cf4ca7cf_10/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1"

finp <- "gis_mapping/D3Po_maps/datasets/london_boroughs.geojson"

if (!file.exists(finp)) {
  download.file(url, destfile = finp, mode = "wb")
}


# Read the GeoJSON file using st_read() from the sf package and 
# clean the column names with clean_names() from the janitor package:

boroughs <- st_read(finp) %>%
  clean_names()

boroughs

# Extract the borough names:
  
names1 <- pull(boroughs, borough)

names1


# Now we need the population for the 33 boroughs

url_pop <- "https://www.citypopulation.de/en/uk/greaterlondon/"

finp2 <- "gis_mapping/D3Po_maps/datasets/london_population.rds"

if (file.exists(finp2)) {
  pop_table <- readRDS(finp2)
} else {
  page <- read_html(url_pop)
  
  tables <- page %>% html_nodes("table")
  
  pop_table <- tables[[1]] %>%
    html_table() %>%
    clean_names()
  
  pop_table <- pop_table %>%
    select(name, pop = population_estimate2024_06_30)
  
  pop_table <- pop_table %>%
    mutate(pop = as.numeric(gsub(",", "", pop)))
  
  names2 <- pull(pop_table, name)
  
  names2
  
  # names that do not match
  setdiff(names1, names2)
  setdiff(names2, names1)
  
  # replace the " and " with " & " in boroughs
  # replace "City of Westminster" with "Westminster"
  pop_table <- pop_table %>%
    mutate(borough = case_when(
      name == "City of Westminster" ~ "Westminster",
      grepl(" and ", name) ~ gsub(" and ", " & ", name),
      TRUE ~ name
    )) %>%
    select(-name)
  
  saveRDS(pop_table, finp2)
}


# Up to this point we can show two maps:

# Inhabitants per borough
# Inhabitants per square km


boroughs <- boroughs %>%
  left_join(pop_table, by = "borough") %>%
  mutate(
    area_km2 = hectares / 100,
    pop_per_km2 = pop / area_km2
  )


# Define a color gradient for the maps a and create the maps using d3po:


my_gradient <- c("#b2d8d8", "#66b2b2", "#008080", "#006666", "#004c4c")

d3po(boroughs, width = 800, height = 600) |>
  po_geomap(
    daes(group = borough, 
         size = pop, 
         color = borough, 
         gradient = TRUE, 
         tooltip = borough)) |>
  po_labels(
    title = "Population in London Boroughs (2024)",
    subtitle = "Source: CityPopulation.DE & TFL London Boroughs"
  )



d3po(boroughs, width = 800, height = 600) %>%
  po_geomap(daes(group = borough, size = pop_per_km2, color = my_gradient, gradient = T, tooltip = borough)) %>%
  po_labels(
    title = "Population per Sq. Kilometer in London Boroughs (2024)",
    subtitle = "Source: CityPopulation.DE & TFL London Boroughs"
  )




my_gradient <- c("#b2d8d8", "#66b2b2", "#008080", "#006666", "#004c4c")

d3po(boroughs, width = 800, height = 600) |>
  po_geomap(
    daes(
      group = borough,
      size  = pop,
      color = pop,             # gradient can only be applied to numeric values
      tooltip = borough
    ),
    gradient = my_gradient      # supply the gradient palette here
  ) |>
  po_labels(
    title = "Population in London Boroughs (2024)",
    subtitle = "Source: CityPopulation.DE & TFL London Boroughs"
  )
