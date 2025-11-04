# Population Treemap

# Website: https://gist.github.com/z3tt/429683594073f7b89b4c50e27c6f1e06

library(ggplot2)
library(treemapify)
library(ggtext)
library(prismatic)

owid_urban <- readr::read_csv("https://www.ggplot2-uncharted.com/data/hyde.csv")

write_csv(owid_urban, "pop_treemap/owid_urban.csv")

treemap <- owid_urban |> 
  sf::st_drop_geometry() |> 
  filter(!is.na(continent)) |> 
  mutate(angle = ifelse(continent == "Oceania", 90, 0)) |> 
  ggplot(aes(area = pop_est, fill = continent, subgroup = continent, label = country)) +
  geom_treemap(color = "#FFFFFF59", size = .8, layout = "scol", start = "topleft") +
  geom_treemap_subgroup_border(color = "white", size = 1.6, layout = "scol", start = "topleft") +
  theme_minimal(base_family = "Londrina Solid", base_size = 16.5) +
  theme(
    legend.position = "none", 
    #plot.margin = margin_auto(10),
    plot.title = ggtext::element_textbox_simple(lineheight = .95, margin = margin(5, 2, 10, 2)),
    plot.caption = ggtext::element_textbox_simple(family = "Affogato", size = rel(.57), lineheight = .95, margin = margin(10, 2, 0, 2))
  )

pal <- c(Africa = "#00978a", Americas = "#7f34a4", Asia = "#d13760", Europe = "#9b6500", Oceania = "#006e9a")

treemap_polished <- treemap +
  geom_treemap_subgroup_text(
    aes(color = stage(continent, after_scale = clr_darken(color, .2)), angle = angle),
    place = "center", layout = "scol", start = "topleft", grow = FALSE, padding.x = unit(.2, "lines"),
    fontface = "plain", family = "Londrina Solid", alpha = .3, size = 120, min.size = 0 
    
  ) +
  geom_treemap_subgroup_text(
    aes(color = continent, angle = angle), 
    place = "center", layout = "scol", start = "topleft", grow = FALSE, padding.x = unit(.2, "lines"), 
    fontface = "plain", family = "Londrina Shadow", alpha = 1, size = 120, min.size = 0
  ) +
  geom_treemap_text(
    aes(alpha = pop_est >= 62845057, color = stage(continent, after_scale = clr_lighten(color, .85))),
    place = "topleft", layout = "scol", start = "topleft", reflow = TRUE, lineheight = .7, 
    family = "Londrina Solid" 
    # family = "Affogato", fontface = "bold"
  ) +
  scale_alpha_manual(values = c(0, .6)) +
  scale_fill_manual(values = clr_lighten(pal, .2)) +
  scale_color_manual(values = clr_darken(pal, .5)) +
  labs(title = "Inhabiting more than 4.7 billion people, <span style='color:#d13760;'>Asian countries</span> constitute roughly 60% of the world's population",
       caption = "<b>The treemap reveals how the world’s population is distributed across continents. Each rectangle shows a country's estimated 2023 population and labels mark the 10% most populous countries.</b><br><br><span style='font-size:9pt;color:#656565;'>Data Source: HYDE (2023) — with minor processing by Our World in Data</span>")

treemap_polished
