# The gt Table package

# Website: https://gt.rstudio.com/index.html

# A) Install package and load library

# install.packages("gt")
library(gt)
library(tidyverse)

# B) Example

# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table based on preprocessed `sp500` table data
sp500 |>
  dplyr::filter(date >= start_date & date <= end_date) |>
  dplyr::select(-adj_close) |>
  gt() |>
  tab_header(
    title = "S&P 500",
    subtitle = glue::glue("{start_date} to {end_date}")
  ) |>
  fmt_currency() |>
  fmt_date(columns = date, date_style = "wd_m_day_year") |>
  fmt_number(columns = volume, suffixing = TRUE)

# C) Introduction to Creating gt Tables

# The gt package is all about making it simple to produce nice-looking 
# display tables.

# A Walkthrough of the gt Basics with a Simple Table
# Website: https://gt.rstudio.com/articles/gt.html

# Take the `islands` dataset and use some dplyr functionality to obtain the ten
# biggest islands in the world

islands_tbl <- 
  tibble(
    name = names(islands),
    size = islands
  ) |>
  arrange(desc(size)) |>
  slice(1:10)

# Display the table
islands_tbl

# Create a display table showing ten of the largest islands in the world
gt_tbl <- gt(islands_tbl)

# Show the gt Table
gt_tbl

# D) Adding Parts to this Simple Table

# The parts (roughly from top to bottom) are:

  # the Table Header (optional; with a title and possibly a subtitle)
  # the Stub and the Stub Head (optional; contains row labels, optionally within row groups having row group labels and possibly summary labels when a summary is present)
  # the Column Labels (contains column labels, optionally under spanner column labels)
  # the Table Body (contains columns and rows of cells)
  # the Table Footer (optional; possibly with footnotes and source notes)

# Make a display table with the `islands_tbl`
# table; put a heading just above the column labels
gt_tbl <- 
  gt_tbl |>
  tab_header(
    title = "Large Landmasses of the World",
    subtitle = "The top ten largest are presented"
  )

# Show the gt Table
gt_tbl

# E) The header

# Use markdown for the heading's `title` and `subtitle` to
# add bold and italicized characters
gt(islands_tbl[1:2,]) |>
  tab_header(
    title = md("**Large Landmasses of the World**"),
    subtitle = md("The *top two* largest are presented")
  )

# F) Source Notes

# A source note can be added to the table’s footer

# Display the `islands_tbl` data with a heading and
# two source notes
gt_tbl <- 
  gt_tbl |>
  tab_source_note(
    source_note = "Source: The World Almanac and Book of Facts, 1975, page 406."
  ) |>
  tab_source_note(
    source_note = md("Reference: McNeil, D. R. (1977) *Interactive Data Analysis*. Wiley.")
  )

# Show the gt table
gt_tbl


# G) Footnotes

# Footnotes live inside the Footer part and their footnote marks are 
# attached to cell data

# Add footnotes (the same text) to two different
# cell; data cells are targeted with `data_cells()`
gt_tbl <- 
  gt_tbl |>
  tab_footnote(
    footnote = "The Americas.",
    locations = cells_body(columns = name, rows = 3:4)
  )

# Show the gt table
gt_tbl

# Determine the row that contains the
# largest landmass ('Asia')
largest <- 
  islands_tbl |> 
  arrange(desc(size)) |>
  slice(1) |>
  pull(name)

# Create two additional footnotes, using the
# `columns` and `where` arguments of `data_cells()`
gt_tbl <- 
  gt_tbl |>
  tab_footnote(
    footnote = md("The **largest** by area."),
    locations = cells_body(
      columns = size,
      rows = name == largest
    )
  ) |>
  tab_footnote(
    footnote = "The lowest by population.",
    locations = cells_body(
      columns = size,
      rows = size == min(size)
    )
  )

# Show the gt table
gt_tbl

# H) The Stub

# The Stub is the area to the left in a table that contains row labels, 
# and may contain row group labels, and summary labels.

# Create a gt table showing ten of the
# largest islands in the world; this
# time with a stub
gt_tbl <- 
  islands_tbl |>
  gt(rowname_col = "name")

# Show the gt table
gt_tbl

# Stubhead label

# Generate a simple table with a stub
# and add a stubhead label
gt_tbl <- 
  gt_tbl |>
  tab_stubhead(label = "landmass")

# Show the gt table
gt_tbl

# Stub and Column labels

# Display the `islands_tbl` data with a stub,
# a heading, source notes, and footnotes
gt_tbl <- 
  gt_tbl |>
  tab_header(
    title = "Large Landmasses of the World",
    subtitle = "The top ten largest are presented"
  ) |>
  tab_source_note(
    source_note = "Source: The World Almanac and Book of Facts, 1975, page 406."
  ) |>
  tab_source_note(
    source_note = md("Reference: McNeil, D. R. (1977) *Interactive Data Analysis*. Wiley.")
  ) |>
  tab_footnote(
    footnote = md("The **largest** by area."),
    locations = cells_body(
      columns = size, rows = largest
    )
  ) |>
  tab_footnote(
    footnote = "The lowest by population.",
    locations = cells_body(
      columns = size, rows = contains("arc")
    )
  )

# Show the gt table
gt_tbl

# Create row groups

# Create three row groups with the
# `tab_row_group()` function
gt_tbl <- 
  gt_tbl |> 
  tab_row_group(
    label = "continent",
    rows = 1:6
  ) |>
  tab_row_group(
    label = "country",
    rows = c("Australia", "Greenland")
  ) |>
  tab_row_group(
    label = "subregion",
    rows = c("New Guinea", "Borneo")
  )

# Show the gt table
gt_tbl

# I) The Column Labels

# The table’s Column Labels part contains, at a minimum, columns and their column labels.

# Modify the `airquality` dataset by adding the year
# of the measurements (1973) and limiting to 10 rows
airquality_m <- 
  airquality |>
  mutate(Year = 1973L) |>
  slice(1:10)

# Create a display table using the `airquality`
# dataset; arrange columns into groups
gt_tbl <- 
  gt(airquality_m) |>
  tab_header(
    title = "New York Air Quality Measurements",
    subtitle = "Daily measurements in New York City (May 1-10, 1973)"
  ) |>
  tab_spanner(
    label = "Time",
    columns = c(Year, Month, Day)
  ) |>
  tab_spanner(
    label = "Measurement",
    columns = c(Ozone, Solar.R, Wind, Temp)
  )

# Show the gt table
gt_tbl

# Rearrange columns

# Move the time-based columns to the start of
# the column series; modify the column labels of
# the measurement-based columns
gt_tbl <- 
  gt_tbl |>
  cols_move_to_start(
    columns = c(Year, Month, Day)
  ) |>
  cols_label(
    Ozone = html("Ozone,<br>ppbV"),
    Solar.R = html("Solar R.,<br>cal/m<sup>2</sup>"),
    Wind = html("Wind,<br>mph"),
    Temp = html("Temp,<br>&deg;F")
  )

# Show the gt table
gt_tbl

########## Put brackets ########

gt_tbl <- 
  gt_tbl |>
  cols_move_to_start(
    columns = c(Year, Month, Day)
  ) |>
  cols_label(
    Ozone = html("Ozone (ppbV)"),
    Solar.R = html("Solar R. (cal/m<sup>2</sup>)"),
    Wind = html("Wind (mph)"),
    Temp = html("Temp (&deg;F)")
  )

gt_tbl

# Function references
# Website: https://gt.rstudio.com/reference/index.html