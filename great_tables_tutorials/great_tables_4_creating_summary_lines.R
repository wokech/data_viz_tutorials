# The gt Table package

# Articles

# Creating Summary Lines
# Website: https://gt.rstudio.com/articles/creating-summary-lines.html

# A) Install package and load library

# install.packages("gt")
library(gt)
library(tidyverse)

# B) Preparing the Input Data Table

exibble |> gt()

# Create a gt table using the `exibble` dataset
exibble_a <-
  exibble |>
  select(-c(fctr, date, time, datetime)) |>
  gt(rowname_col = "row", groupname_col = "group") |>
  sub_missing()

exibble_a

# C) Generating Group-wise Summary Rows

# Create group-wise summary rows for both
# groups (using `groups = everything()`); use the
# `mean()`, `sum()`, and `sd()` functions
# (only for the `num` column)
exibble_b <- 
  exibble_a |>
  summary_rows(
    groups = everything(),
    columns = num,
    fns = list(
      average = "mean",
      total = "sum",
      SD = "sd"
    )
  )

exibble_b


# Create group-wise summary rows for both
# groups (using `groups = everything()`); we will
# use names and formulas this time in `fns`
exibble_c <- 
  exibble_a |>
  summary_rows(
    groups = everything(),
    columns = num,
    fns = list(
      avg = ~ mean(., na.rm = TRUE),
      total = ~ sum(., na.rm = TRUE),
      s.d. = ~ sd(., na.rm = TRUE)
    )
  )

exibble_c


# Define a named list of aggregation
# functions and summary row labels
fns_labels <- 
  list(
    avg = ~mean(., na.rm = TRUE),
    total = ~sum(., na.rm = TRUE),
    s.d. = ~sd(., na.rm = TRUE)
  )

# Create group-wise summary rows as
# before, supply `fns_labels` to `fns`,
# and format the cell summary data
exibble_d <- 
  exibble_a |>
  fmt_scientific(
    columns = num,
    decimals = 3
  ) |>
  summary_rows(
    groups = everything(),
    columns = num,
    fns = fns_labels,
    fmt = list(~ fmt_scientific(., decimals = 3))
  )

exibble_d


# D) Using Multiple Calls of summary_rows()

# Create group-wise summary rows as
# before, supply `fns_labels` to `fns`,
# and format the cell summary data
exibble_e <- 
  exibble_a |>
  fmt_scientific(
    columns = num,
    decimals = 3
  ) |>
  fmt_currency(
    columns = currency,
    currency = "EUR"
  ) |>
  summary_rows(
    groups = everything(),
    columns = num,
    fns = fns_labels,
    fmt = list(~ fmt_scientific(., decimals = 3))
  ) |>
  summary_rows(
    groups = "grp_a",
    columns = currency,
    fns = c(
      fns_labels,
      min = ~ min(.),
      max = ~ max(.)
    ),
    fmt = list(~ fmt_currency(., currency = "EUR"))
  )

exibble_e


# Provide common formatting parameters to a list
# object named `formats`; the number of decimal
# places will be `2` and the locale is "fr_BE"
formats <- 
  list(
    decimals = 3,
    locale = "fr_BE",
    currency = "EUR"
  )

# Format the `num` and `currency` columns
# (using the values stored in `formats`);
# when generating summary rows we can also
# supply formatter options from this list
exibble_f <- 
  exibble_a |>
  fmt_scientific(
    columns = num,
    decimals = formats$decimals,
    locale = formats$locale
  ) |>
  fmt_currency(
    columns = currency,
    currency = formats$currency,
    locale = formats$locale
  ) |>
  summary_rows(
    groups = everything(),
    columns = num,
    fns = fns_labels,
    fmt = list(~ fmt_scientific(.,
                                decimals = formats$decimals,
                                locale = formats$locale
    ))
  ) |>
  summary_rows(
    groups = "grp_a",
    columns = currency,
    fns = c(
      fns_labels,
      min = ~min(.),
      max = ~max(.)
    ),
    fmt = list(~ fmt_currency(.,
                              currency = formats$currency,
                              locale = formats$locale
    ))
  )

exibble_f


# E) Creating a Grand Summary

# Create a simple grand summary on a gt
# table that contains no stub
exibble_g <-
  exibble |>
  select(num, char, currency) |>
  gt() |>
  grand_summary_rows(
    columns = c(num, currency),
    fns = fns_labels
  )

exibble_g


# Using the table in `exibble_f`, create
# grand summary rows (using two separate
# calls of `grand_summary_rows()` since
# the formatting will be different)
exibble_h <- 
  exibble_f |>
  grand_summary_rows(
    columns = num,
    fns = fns_labels,
    fmt = list(~ fmt_number(.,
                            suffixing = TRUE,
                            locale = formats$locale
    ))
  ) |>
  grand_summary_rows(
    columns = currency,
    fns = fns_labels,
    fmt = list(~ fmt_currency(.,
                              suffixing = TRUE,
                              locale = formats$locale
    ))
  )

exibble_h

# F) Adding Some Style to the Summary Cells

# Using the gt table of `exibble_h` as a
# starting point, style summary cells with
# `tab_options()` and add two footnotes
exibble_i <- 
  exibble_h |>
  tab_options(
    summary_row.background.color = "lightblue",
    grand_summary_row.background.color = "lightgreen"
  ) |>
  tab_footnote(
    footnote = md("Mean of all *num* values."),
    locations = cells_grand_summary(
      columns = "num", rows = "avg"
    )
  ) |>
  tab_footnote(
    footnote = md("Highest `currency` value in **grp_a**"),
    locations = cells_summary(
      groups = "grp_a",
      columns = "currency",
      rows = "max"
    )
  )

exibble_i


# G) Extracting the Summary Data from the gt Table Object

# Extract the summary data from `exibble_d`
# to a list  object
summary_list <- exibble_d |> extract_summary()

# Print out the summary for the `grp_a` group
summary_list$summary_df_data_list$grp_a

# Print out the summary for the `grp_b` group
summary_list$summary_df_data_list$grp_b

# Take `exibble_d`, which internally has a list
# of summary data frames, extract the summaries,
# and then combine them; input that into `gt()`,
# and format the `num` column with `fmt_number()`
exibble_d |>
  extract_summary() |>
  unlist(recursive = FALSE) |>
  bind_rows() |>
  gt() |>
  fmt_number(
    columns = num,
    decimals = 1
  ) |>
  sub_missing(columns = c(char, currency, row, group))

# H) Providing Our Own Aggregation Functions to Generate Summary Rows

# Define a function that gives the
# highest two values above a threshold
agg_highest_two_above_value <- function(x, threshold) {
  
  # Get sorted values above threshold value
  values <- sort(round(x[x >= threshold], 2))
  
  # Return character string with 2 highest values above threshold
  if (length(values) == 0) {
    return(paste0("No values above ", threshold))
  } else {
    return(
      paste(
        formatC(
          tail(
            sort(round(x[x > threshold], 2)), 2),
          format = "f", digits = 2), collapse = ", "))
  }
}

# Let's test this function with some values
agg_highest_two_above_value(
  x = c(0.73, 0.93, 0.75, 0.86, 0.23, 0.81),
  threshold = 0.8
)

# Create a gt table with `exibble_a` and use
# the custom function with a threshold of `20`;
# the `fmt_passthrough` funtion allows for
# minimal formatting of the aggregate values
exibble_j <- 
  exibble_a |>
  grand_summary_rows(
    columns = c(num, currency),
    fns = list(high = ~ agg_highest_two_above_value(., 20)),
    fmt = list(~ fmt_passthrough(., pattern = "({x})"))
  )

exibble_j

# Extract the summary list from `exibble_j`
# and inspect using `str()`
exibble_j |>
  extract_summary() |>
  str()


