# Introducing ggauto: automating better charts
# Nicole Rennie
# https://nrennie.rbind.io/blog/introducing-ggauto/

# Install package

install.packages("ggauto")

# Load the required packages

library(ggauto)
library(ggplot2)

# Visualising distributions

# If you have only continuous variable and you want 
# to visualise the distribution

penguins |>
  ggauto(bill_dep)

# pass directly without a pipe
ggauto(penguins, bill_dep)

# use a vector
ggauto(penguins$bill_dep)

# If you have multiple categories, and you want to 
# visualise the distribution for each of them, 
# i.e., you have one discrete variable, and 
# one continuous variable, then multiple raincloud plots are produced

penguins |>
  dplyr::filter(species == "Adelie") |>
  ggauto(island, flipper_len)

# Visualising data over time

# If you have a single variable to show over time, 
# i.e., one date variable, and one continuous variable, 
# a line chart is produced.

economics_long |>
  dplyr::filter(variable == "unemploy") |>
  ggauto(date, value)

# If you need to show how multiple variables change over time, 
# i.e., one date variable, continuous variable, and one discrete variable, 
# the type of chart will depend on how many 
# categories (unique values in the discrete variable) you have.

# If you have 6 or fewer categories, a multi-line chart is created, 
# with colours and symbols identifying the categories. Category labels 
# are added at the end of each line automatically.

txhousing |>
  dplyr::filter(city %in% c("Houston", "Fort Worth", "San Antonio", "Austin")) |>
  dplyr::mutate(date = lubridate::ymd(paste0(year, "/", month, "/01"))) |>
  ggauto(date, sales, city)

# If you have more than 6 categories, the plot type changes to 
# a faceted line chart, with one category highlighted on each facet:

txhousing |>
  dplyr::filter(city %in% c(
    "Houston", "Fort Worth", "San Antonio", "Austin",
    "Bay Area", "Dallas", "Paris", "San Angelo"
  )) |>
  dplyr::mutate(date = lubridate::ymd(paste0(year, "/", month, "/01"))) |>
  ggauto(date, sales, city)

# Visualising magnitudes and ranks

# If you have a single discrete variable, a bar chart showing 
# the counts of each category is created:

diamonds |>
  ggauto(cut)

# If you have one discrete variable, and one continuous variable 
# with only a single value for each discrete variable, 
# a bar chart of the values is created:

diamonds |>
  dplyr::group_by(cut) |>
  dplyr::summarise(med_price = median(price)) |>
  ggauto(cut, med_price)

# As you can see, when the discrete variable is a factor (i.e. cut), 
# the desired order is respected. If the discrete variable is not a 
# factor, the bars are ordered from highest to lowest instead of 
# the default alphabetical ordering:

diamonds |>
  dplyr::group_by(cut) |>
  dplyr::summarise(med_price = median(price)) |>
  dplyr::mutate(cut = as.character(cut)) |>
  ggauto(cut, med_price)

# If you have two discrete variables, then a heatmap is created 
# showing the count of each combination of categories. 
# Labels are added showing the count.

mpg |>
  dplyr::mutate(cyl = as.character(cyl)) |>
  ggauto(cyl, drv)

# If there are more than 6 categories on either axis, 
# labels are replaced with a legend:

txhousing |>
  dplyr::filter(median >= 150000, year >= 2005) |>
  dplyr::mutate(
    month = factor(month, levels = 1:12),
    year = factor(year, levels = 2005:2015)
  ) |>
  ggauto(month, year)

# Again, if one or both of the discrete variables is a factor, 
# then the order is respected. If not, the categories are 
# ordered by magnitude (based on the sum).

txhousing |>
  dplyr::filter(median >= 150000, year >= 2005) |>
  dplyr::mutate(
    month = as.character(month),
    year = factor(year, levels = 2005:2015)
  ) |>
  ggauto(month, year)

# If you have two discrete variables and a third continuous variable 
# showing some summary statistic for each category combination, 
# a heatmap showing that value is created. 
# Labels are rounded to 2 decimal places.

mpg |>
  dplyr::mutate(cyl = as.character(cyl)) |>
  dplyr::group_by(cyl, drv) |>
  dplyr::summarise(mean_hwy = mean(hwy)) |>
  dplyr::ungroup() |>
  ggauto(cyl, drv, mean_hwy)

# If there are multiple continuous values per combination of categories, 
# an error is returned, asking you to first summarise the data:

mpg |>
  dplyr::mutate(cyl = as.character(cyl)) |>
  ggauto(cyl, drv, hwy)

# Visualising correlation

# To show the correlation between two continuous variables:

mpg |>
  ggauto(displ, hwy)

# To show the correlation between two continuous variables, 
# split by a third discrete variable, a scatter plot using 
# colours and shapes is created:

mpg |>
  dplyr::mutate(cyl = as.factor(cyl)) |>
  ggauto(displ, hwy, cyl)

# If you try to use more than 6 colours (categories), 
# the chart type changes to a faceted scatter plot with one category 
# highlighted on each facet:

mpg |>
  dplyr::mutate(cyl = as.factor(cyl)) |>
  ggauto(displ, hwy, manufacturer)

# Comparing to ggplot2

plot_data <- txhousing |>
  dplyr::filter(city %in% c(
    "Houston", "Fort Worth", "San Antonio", "Austin",
    "Bay Area", "Dallas", "Paris", "San Angelo"
  )) |>
  dplyr::mutate(date = lubridate::ymd(paste0(year, "/", month, "/01")))

plot_data |>
  ggplot() +
  geom_line(aes(x = date, y = sales, colour = city))

plot_data |>
  ggauto(date, sales, city)

# Editing charts

# Scales

set.seed(123)
plot_data <- data.frame(
  v1 = rnorm(50, 1)
)

ggauto(plot_data, v1) +
  scale_x_continuous()

# Text

plot_data |>
  ggauto(v1,
         title = "Descriptive title goes here",
         subtitle = "More information about what's in the chart which can be a really, really long sentence that will wrap onto multiple lines automatically.",
         caption = "**Source**: where the data is from",
         xlab = "Nice variable name"
  )


