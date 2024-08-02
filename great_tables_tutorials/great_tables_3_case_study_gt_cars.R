# The gt Table package

# Articles

# Case Study: gtcars
# Website:  https://gt.rstudio.com/articles/case-study-gtcars.html

# A) Install package and load library

# install.packages("gt")
library(gt)
library(tidyverse)

# Let’s make a display table using the gtcars dataset

glimpse(gtcars)

# Get a subset of 8 cars from the `gtcars` dataset: two
# from each manufacturer country of origin except the UK
gtcars_8 <-
  gtcars |>
  group_by(ctry_origin) |>
  slice_head(n = 2) |>
  ungroup() |>
  filter(ctry_origin != "United Kingdom")

# Show the `gtcars_8` tibble
glimpse(gtcars_8)

# B) Row Groups

# Use `group_by()` on `gtcars` and pass that to `gt()`
gtcars_8 |>
  group_by(ctry_origin) |>
  gt()

# Order row groups

gtcars_8 |>
  group_by(ctry_origin) |>
  arrange(mfr, desc(msrp)) |>
  gt()

# Define our preferred order for `ctry_origin`
order_countries <- c("Germany", "Italy", "United States", "Japan")

# Reorder the table rows by our specific ordering of groups
gtcars_8 |>
  arrange(
    factor(ctry_origin, levels = order_countries), mfr, desc(msrp)
  ) |>
  group_by(ctry_origin) |>
  gt()

#  Select and mutate

# Reorder the table rows by our specific ordering of groups
tab <-
  gtcars_8 |>
  arrange(
    factor(ctry_origin, levels = order_countries),
    mfr, desc(msrp)
  ) |>
  mutate(car = paste(mfr, model)) |>
  select(-mfr, -model) |>
  group_by(ctry_origin) |>
  gt(rowname_col = "car")

# Show the table
tab

# C) Hiding and Moving Some Columns

# Use a few `cols_*()` functions to hide and move columns 
tab <- 
  tab |>
  cols_hide(columns = c(drivetrain, bdy_style)) |>
  cols_move(
    columns = c(trsmn, mpg_c, mpg_h),
    after = trim
  )

# Show the table
tab

# D) Putting Columns Into Groups

# Put the first three columns under a spanner
# column with the label 'Performance'
tab <- 
  tab |>
  tab_spanner(
    label = "Perform",
    columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
  )

# Show the table
tab

# E) Merging Columns Together and Labeling Them

# Perform three column merges to better present
# MPG, HP, and torque; relabel all the remaining
# columns for a nicer-looking presentation
tab <- 
  tab |>
  cols_merge(
    columns = c(mpg_c, mpg_h),
    pattern = "<<{1}c<br>{2}h>>"
  ) |>
  cols_merge(
    columns = c(hp, hp_rpm),
    pattern = "{1}<br>@{2}rpm"
  ) |>
  cols_merge(
    columns = c(trq, trq_rpm),
    pattern = "{1}<br>@{2}rpm"
  ) |>
  cols_label(
    mpg_c = "MPG",
    hp = "HP",
    trq = "Torque",
    year = "Year",
    trim = "Trim",
    trsmn = "Transmission",
    msrp = "MSRP"
  )

# Show the table
tab

# F) Using Formatter Functions

# Format the `msrp` column to USD currency
# with no display of the currency subunits
tab <- 
  tab |>
  fmt_currency(columns = msrp, decimals = 0)

# Show the table
tab

# G) Column Alignment and Style Changes

# Center-align three columns in the gt table and
# modify the text size of a few columns of data
tab <- 
  tab |>
  cols_align(
    align = "center",
    columns = c(mpg_c, hp, trq)
  ) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = c(trim, trsmn, mpg_c, hp, trq)
    )
  )

# Show the table
tab

# H) Text Transforms

# Transform the column of text in `trsmn` using
# a custom function within `text_transform()`;
# here `x` represents a character vector defined
# in the `cells_body()` function
tab <- 
  tab |>
  text_transform(
    locations = cells_body(columns = trsmn),
    fn = function(x) {
      
      # The first character of `x` always
      # indicates the number of transmission speeds
      speed <- substr(x, 1, 1)
      
      # We can carefully determine which transmission
      # type we have in `x` with a `dplyr::case_when()`
      # statement
      type <-
        dplyr::case_when(
          substr(x, 2, 3) == "am" ~ "Automatic/Manual",
          substr(x, 2, 2) == "m" ~ "Manual",
          substr(x, 2, 2) == "a" ~ "Automatic",
          substr(x, 2, 3) == "dd" ~ "Direct Drive"
        )
      
      # Let's paste together the `speed` and `type`
      # vectors to create HTML text replacing `x`
      paste(speed, " Speed<br><em>", type, "</em>")
    }
  )

# Show the table
tab

# I) Table Header: Title and Subtitle

# Add a table title and subtitle; we can use
# markdown with the `md()` helper function
tab <- 
  tab |>
  tab_header(
    title = md("The Cars of **gtcars**"),
    subtitle = "These are some fine automobiles"
  )

# Show the table
tab

# J) Adding a Source Citation

# Add a source note to the bottom of the table; this
# appears below the footnotes
tab <- 
  tab |>
  tab_source_note(
    source_note = md(
      "Source: Various pages within the Edmonds website."
    )
  )

# Show the table
tab


# K) Using the Complete gtcars table and Adding Footnotes


# Use dplyr functions to get the car with the best city gas mileage;
# this will be used to target the correct cell for a footnote
best_gas_mileage_city <- 
  gtcars |> 
  arrange(desc(mpg_c)) |>
  slice(1) |>
  mutate(car = paste(mfr, model)) |>
  pull(car)

# Use dplyr functions to get the car with the highest horsepower
# this will be used to target the correct cell for a footnote
highest_horsepower <- 
  gtcars |> 
  arrange(desc(hp)) |>
  slice(1) |>
  mutate(car = paste(mfr, model)) |>
  pull(car)

# Define our preferred order for `ctry_origin`
order_countries <- c("Germany", "Italy", "United States", "Japan")

# Create a display table with `gtcars`, using all of the previous
# statements piped together + additional `tab_footnote()` stmts
tab <-
  gtcars |>
  arrange(
    factor(ctry_origin, levels = order_countries),
    mfr, desc(msrp)
  ) |>
  mutate(car = paste(mfr, model)) |>
  select(-mfr, -model) |>
  group_by(ctry_origin) |>
  gt(rowname_col = "car") |>
  cols_hide(columns = c(drivetrain, bdy_style)) |>
  cols_move(
    columns = c(trsmn, mpg_c, mpg_h),
    after = trim
  ) |>
  tab_spanner(
    label = "Performance",
    columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
  ) |>
  cols_merge(
    columns = c(mpg_c, mpg_h),
    pattern = "<<{1}c<br>{2}h>>"
  ) |>
  cols_merge(
    columns = c(hp, hp_rpm),
    pattern = "{1}<br>@{2}rpm"
  ) |>
  cols_merge(
    columns = c(trq, trq_rpm),
    pattern = "{1}<br>@{2}rpm"
  ) |>
  cols_label(
    mpg_c = "MPG",
    hp = "HP",
    trq = "Torque",
    year = "Year",
    trim = "Trim",
    trsmn = "Transmission",
    msrp = "MSRP"
  ) |>
  fmt_currency(columns = msrp, decimals = 0) |>
  cols_align(
    align = "center",
    columns = c(mpg_c, hp, trq)
  ) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = c(trim, trsmn, mpg_c, hp, trq)
    )
  ) |>
  text_transform(
    locations = cells_body(columns = trsmn),
    fn = function(x) {
      
      speed <- substr(x, 1, 1)
      
      type <-
        dplyr::case_when(
          substr(x, 2, 3) == "am" ~ "Automatic/Manual",
          substr(x, 2, 2) == "m" ~ "Manual",
          substr(x, 2, 2) == "a" ~ "Automatic",
          substr(x, 2, 3) == "dd" ~ "Direct Drive"
        )
      
      paste(speed, " Speed<br><em>", type, "</em>")
    }
  ) |>
  tab_header(
    title = md("The Cars of **gtcars**"),
    subtitle = "These are some fine automobiles"
  ) |>
  tab_source_note(
    source_note = md(
      "Source: Various pages within the Edmonds website."
    )
  ) |>
  tab_footnote(
    footnote = md("Best gas mileage (city) of all the **gtcars**."),
    locations = cells_body(
      columns = mpg_c,
      rows = best_gas_mileage_city
    )
  ) |>
  tab_footnote(
    footnote = md("The highest horsepower of all the **gtcars**."),
    locations = cells_body(
      columns = hp,
      rows = highest_horsepower
    )
  ) |>
  tab_footnote(
    footnote = "All prices in U.S. dollars (USD).",
    locations = cells_column_labels(columns = msrp)
  )

# Show the table
tab
