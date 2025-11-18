# Warming Stripes
# Andrew Heiss

library(tidyverse)
library(ggview)

# Download the annual HadCRUT.5.1.0.0 data from
# https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.1.0.0/download.htm

hadcrut <- read_csv(
  "global_warming_stripes/HadCRUT.5.1.0.0.analysis.summary_series.global.annual.csv"
) |>
  rename(
    anomaly = `Anomaly (deg C)`,
    conf_low = `Lower confidence limit (2.5%)`,
    conf_high = `Upper confidence limit (97.5%)`
  )



# Get min, max, and mean for anomaly values for rescaling and centering the legend at 0
scaling <- hadcrut |> 
  summarize(max = max(anomaly), min = min(anomaly), mean = mean(anomaly))

# Plot the warming stripes
ggplot(hadcrut, aes(x = Time, y = 1, fill = anomaly)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(11, "RdBu")),
    values = scales::rescale(c(scaling$min, scaling$mean, scaling$max)),
    guide = "none"
  ) +
  theme_void() +
  ggview::canvas(width = 8, height = 2)

# Bars for fun
ggplot(hadcrut, aes(x = Time, y = anomaly, fill = anomaly)) +
  geom_col() +
  scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(11, "RdBu")),
    values = scales::rescale(c(scaling$min, scaling$mean, scaling$max)),
    guide = "none"
  ) +
  theme_void() +
  ggview::canvas(width = 8, height = 2)
