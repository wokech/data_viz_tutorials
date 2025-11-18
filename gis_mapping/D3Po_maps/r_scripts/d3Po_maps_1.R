# Creating a London Population Map with D3po

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

finp <- "~/datasets/london_boroughs.geojson"

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

