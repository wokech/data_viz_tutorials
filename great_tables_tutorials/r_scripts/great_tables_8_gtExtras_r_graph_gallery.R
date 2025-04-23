# Enhance gt tables with gtExtras

# Website: https://r-graph-gallery.com/package/gtextras.html

# A) Install packages

# install.packages("gtExtras")

# B) Examples

# Basic Usage

library(gtExtras)

head(penguins) %>% 
  gt() %>% 
  gt_theme_excel()

# Key features

# Theming

head(iris) %>% 
  gt() %>% 
  gt_theme_dot_matrix()

# Color rows

head(iris) %>%
  gt() %>%
  gt_color_rows(
    1:4, # column 1,2,3 and 4
    palette = "RColorBrewer::Spectral"
  )

# Highlight specific row

head(iris, 6) %>%
  gt() %>%
  gt_highlight_rows(
    rows = c(1, 4), # rows to highlight
    target_col = 3, # which column to focus on
    bold_target_only = TRUE, # highlight target column 
    fill='darkred', # background color
    font_color = "#000051", # text color,
    alpha=0.5, # controls color opacity
  )

# Plotting

iris %>%
  group_by(Species) %>%
  summarize(SL_data = list(Sepal.Length)) %>%
  gt() %>%
  gt_plt_dist(SL_data)


iris %>%
  group_by(Species) %>%
  summarize(SL_data = list(Sepal.Length)) %>%
  gt() %>%
  gt_plt_sparkline(SL_data)

#### Plotting inside gt table with gtExtras

# load packages
library(gt)
library(gtExtras)
library(dplyr)

# load the dataset
data(iris)

# create aggregated dataset
agg_iris = iris %>%
  group_by(Species) %>%
  summarize(
    Sepal.L = list(Sepal.Length),
    Sepal.W = list(Sepal.Width),
    Petal.L = list(Petal.Length),
    Petal.W = list(Petal.Width)
  )

# display the table with default output with gt package
agg_iris %>%
  gt()


# Line chart

agg_iris %>%
  gt() %>%
  gt_plt_sparkline(Sepal.L) %>%
  gt_plt_sparkline(Sepal.W) %>%
  gt_plt_sparkline(Petal.L) %>%
  gt_plt_sparkline(Petal.W)


# Distribution chart

agg_iris %>%
  gt() %>%
  gt_plt_dist(
    Sepal.L,
    type = "density" 
  ) %>%
  gt_plt_dist( 
    Sepal.W,
    type = "boxplot"
  ) %>%
  gt_plt_dist( 
    Petal.L,
    type = "histogram"
  ) %>%
  gt_plt_dist(
    Petal.W,
    type = "rug_strip"
  )

# Bar chart

head(iris) %>%
  gt() %>%
  gt_plt_bar_pct(
    Sepal.Length,
    labels = TRUE
  ) %>%
  gt_plt_bar_pct(
    Sepal.Width,
    labels=FALSE,
    fill = "forestgreen"
  )

# Summary chart

iris %>%
  gt_plt_summary()


##### Change the theme of gt tables with gtExtras

# Default theme

data(mtcars)

head(mtcars) %>%
  gt()

# Excel theme

head(mtcars) %>%
  gt() %>%
  gt_theme_excel()

# FiveThirtyEight theme

head(mtcars) %>%
  gt() %>%
  gt_theme_538()

# ESPN theme

head(mtcars) %>%
  gt() %>%
  gt_theme_espn()


# NY Times theme

head(mtcars) %>%
  gt() %>%
  gt_theme_nytimes()

# Dot matrix theme

head(mtcars) %>%
  gt() %>%
  gt_theme_dot_matrix()

# Dark theme

head(mtcars) %>%
  gt() %>%
  gt_theme_dark()

# PFF theme

head(mtcars) %>%
  gt() %>%
  gt_theme_pff()

# Guardian theme

head(mtcars) %>%
  gt() %>%
  gt_theme_guardian()
