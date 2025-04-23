## Albert Rapp US Map tutorial

library(tidyverse)
library(usmap)

plot_usmap(fill = "grey20", color = "white", regions = "states")

View(countypop)

plot_usmap(
  linewidth = 0.1,
  color = "white",
  regions = "counties",
  data = countypop,
  values = "pop_2022"
  ) + 
  scale_fill_gradient(
    trans = "log",
    labels = scales::label_number(scale = 1/1000, suffix = "k", big.mark = ","),
    breaks = c(1000, 20000, 400000, 8000000),
    high = "#0072B2",
    low = "white"
  ) + 
  theme(
    text = element_text(family = "Source Sans Pro", size = 16),
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)) + 
  labs(fill  = "Population (2022)") +
  guides(
    fill = guide_colorbar(
      barwidth = unit(10, "cm")
    )
  )

ggsave("gis_mapping/us_maps/gis_map_us_1.png",
       width = 8, height = 4, dpi = 300)
