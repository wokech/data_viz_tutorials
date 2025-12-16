# Flourish Charts
# Creating Flourish graphs in R and Python

# https://www.canva.dev/opensource/flourish-charts/quickstart/

library(flourishcharts)
library(tidyr)
library(dplyr)

head(flourish_api_documentation)


# Scatterplot

scatterplot <- flourish(
  chart_type = "scatter"
) |>
  bind_scatter_data(
    data = gapminder,
    x = "gdpPercap",
    y = "lifeExp",
    slider = "year",
    size = "pop",
    color = "continent",
    metadata = c("country", "year")
  )
scatterplot

# Bar chart race

bcr_data <- gapminder |>
  filter(country %in% c(
    "Australia",
    "New Zealand",
    "United States",
    "Rwanda",
    "Sierra Leone",
    "Indonesia",
    "Brazil"
  )) |>
  select(c("country", "continent", "year", "lifeExp")) |>
  pivot_wider(
    id_cols = c("country", "continent"),
    names_from = "year",
    values_from = "lifeExp"
  )

bcr <- flourish("bar_race") |>
  bind_bar_chart_race_data(
    data = bcr_data,
    label = "country",
    values = colnames(bcr_data[, c(3:14)]),
    category = "continent"
  ) |>
  set_bar_chart_race_details(
    chart_layout_title = "Life expectancy from the 1950s to 2007",
    chart_layout_subtitle = "Selected countries include Australia, New Zealand, the US, Rwanda, Indonesia, Sierra Leone, and Brazil.",
    totaliser = FALSE
  )
bcr

# Alluvial Diagram

alluvial_data <- tibble::tribble(
  ~before,~after,~seat_flow,
  "Radicals","Radicals" , 27,
  "Moderates","Moderates",1,
  "Green","Green",4,
  "Progressives","Progressives",15,
  "Radicals","Moderates",28,
  "Progressives","Radicals",12,
  "Libertarians","Libertarians",8,
  "Moderates","Radicals",10,
  "Progressives","Moderates",6,
  "Radicals","Independents",5,
  "Independents","Independents",4,
  "Progressives","Independents",3,
  "Progressives","Libertarians",7,
  "Independents","Moderates",2,
  "Loonies","Green",2,
  "Independents","Radicals",1,
  "Loonies","Libertarians",10,
  "Independents","Independents",1
)

alluvial_graph <- flourish(chart_type = "sankey") |>
  bind_sankey_data(data = alluvial_data,
                   source = "before",
                   target = "after",
                   value = "seat_flow"
  )
alluvial_graph
