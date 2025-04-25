# ggplot2 theme tutorial

library(ggplot2)

# 1. Original plot

default_plot <- ggplot(
  economics,
  aes(date, unemploy)
) +
  geom_line() +
  labs(
    title = "Unemployed persons in the States",
    subtitle = "Monthly aggregation (1967 - 2015)",
    caption = "Data source: {ggplot2} R package",
    x = NULL,
    y = NULL
  ) +
  # Change the scale of the y-axis to numbers in thousands rather
  # than millions
  scale_y_continuous(
    labels = scales::label_number(
      scale = 0.001,
      suffix = "M"
    ),
    limits = c(0, max(economics$unemploy))
  )

default_plot

# 2. Pick a built-in theme as a base

nyt_plot <- default_plot + 
  theme_minimal(
    base_family = "Libre Franklin"
  )

nyt_plot

# 3. Customize text elements

nyt_plot <- nyt_plot + 
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      color = "#333333", # dark gray
      margin = margin(b = 10) # gives breathing room in plot
    ),
    plot.subtitle = element_text(
      size = 14,
      color = "#999999", # medium gray
      margin = margin(b = 10) # gives breathing room in plot
    ),
    plot.caption = element_text(
      size = 13,
      color = "#777777", # light gray
      margin = margin(t = 15),
      hjust = 0 # 0 equals to left align and 1 equals to right align
    ),
    axis.text = element_text(
      size = 11,
      color = "#333333" # dark
    )
  )

nyt_plot

# 4. Align title, subtitle, and caption to plot

nyt_plot <- nyt_plot + 
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

nyt_plot

# 5. Customize line elements

nyt_plot <- nyt_plot +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linetype = "dashed",
      linewidth = 0.15,
      color = "#999999"
    ),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(
      linetype = "solid",
      linewidth = 0.25,
      color = "#999999"
    ),
    axis.ticks.length.x = unit(4, units = "pt")
  )

nyt_plot

# Wrap it in a function
theme_nyt <- function() {

# Pick a built-in theme as a base 
  
    theme_minimal(
    base_family = "Libre Franklin"
  ) +

# Customize text elements

    theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        color = "#333333", # dark gray
        margin = margin(b = 10) # gives breathing room in plot
      ),
      plot.subtitle = element_text(
        size = 14,
        color = "#999999", # medium gray
        margin = margin(b = 10) # gives breathing room in plot
      ),
      plot.caption = element_text(
        size = 13,
        color = "#777777", # light gray
        margin = margin(t = 15),
        hjust = 0 # 0 equals to left align and 1 equals to right align
      ),
      axis.text = element_text(
        size = 11,
        color = "#333333" # dark
      ),

# Align title, subtitle, and caption to plot

      plot.title.position = "plot",
      plot.caption.position = "plot",

# Customize line elements

      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        linetype = "dashed",
        linewidth = 0.15,
        color = "#999999"
      ),
      panel.grid.major.x = element_blank(),
      axis.ticks.x = element_line(
        linetype = "solid",
        linewidth = 0.25,
        color = "#999999"
      ),
      axis.ticks.length.x = unit(4, units = "pt")
    )
}

# Test function

default_plot + 
  theme_nyt()

# Customize the new theme function by adding arguments

theme_nyt <- function(
    gridline_x = TRUE,
    gridline_y = TRUE
) {
  gridline <- element_line(
    linetype = "dashed",
    linewidth = 0.15,
    color = "#999999"
  )
  
gridline_x <- if (isTRUE(gridline_x)) {
  gridline
} else {
  element_blank()
}

gridline_y <- if (isTRUE(gridline_y)) {
  gridline
} else {
  element_blank()
}

# Pick a built-in theme as a base 

theme_minimal(
  base_family = "Libre Franklin"
) +
  
  # Customize text elements
  
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      color = "#333333", # dark gray
      margin = margin(b = 10) # gives breathing room in plot
    ),
    plot.subtitle = element_text(
      size = 14,
      color = "#999999", # medium gray
      margin = margin(b = 10) # gives breathing room in plot
    ),
    plot.caption = element_text(
      size = 13,
      color = "#777777", # light gray
      margin = margin(t = 15),
      hjust = 0 # 0 equals to left align and 1 equals to right align
    ),
    axis.text = element_text(
      size = 11,
      color = "#333333" # dark
    ),
    
    # Align title, subtitle, and caption to plot
    
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    # Customize line elements
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = gridline_x,
    panel.grid.major.y = gridline_y,
    axis.ticks.x = element_line(
      linetype = "solid",
      linewidth = 0.25,
      color = "#999999"
    ),
    axis.ticks.length.x = unit(4, units = "pt")
  )

}

# Test our function with grid line arguments

default_plot + 
  theme_nyt(
    gridline_x = TRUE,
    gridline_y = TRUE
  )

# Create a new test plot

mtcars_plot <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  labs(
    title = "Fuel economy declines as weight increases",
    subtitle = "Fuel economy and weight for 32 automobiles (1973-1974 models)",
    caption = "Data source: {ggplot2} R package",
    x = "Weight (1000 lbs)",
    y = "Miles per US gallon"
  )

mtcars_plot

# Test our function on new plot

mtcars_plot + 
  theme_nyt(
    gridline_x = TRUE,
    gridline_y = TRUE
  )
