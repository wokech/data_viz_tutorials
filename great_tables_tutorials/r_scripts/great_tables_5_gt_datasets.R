# The gt Table package

# Articles

# gt Datasets

# Website: https://gt.rstudio.com/articles/gt-datasets.html

# A) Install package and load library

# install.packages("gt")
library(gt)
library(tidyverse)

# The gt package comes with six built-in datasets for experimenting 
# with the gt API: countrypops, sza, gtcars, sp500, pizzaplace, and exibble.

# B) countrypops: This dataset provides the total populations of 
# 215 countries on a yearly basis, from 1960 to 2021.

# Example

# Get vectors of 2-letter country codes for each region of Oceania
Australasia <- c("AU", "NZ")
Melanesia <- c("NC", "PG", "SB", "VU")
Micronesia <- c("FM", "GU", "KI", "MH", "MP", "NR", "PW")
Polynesia <- c("PF", "WS", "TO", "TV")

# Create a gt table based on a preprocessed `countrypops`
countrypops |>
  filter(country_code_2 %in% c(
    Australasia, Melanesia, Micronesia, Polynesia)
  ) |>
  filter(year %in% c(2000, 2010, 2020)) |>
  mutate(region = case_when(
    country_code_2 %in% Australasia ~ "Australasia",
    country_code_2 %in% Melanesia ~ "Melanesia",
    country_code_2 %in% Micronesia ~ "Micronesia",
    country_code_2 %in% Polynesia ~ "Polynesia",
  )) |>
  pivot_wider(names_from = year, values_from = population) |>
  arrange(region, desc(`2020`)) |>
  select(-starts_with("country_code")) |>
  gt(
    rowname_col = "country_name",
    groupname_col = "region"
  ) |>
  tab_header(title = "Populations of Oceania's Countries in 2000, 2010, and 2020") |>
  tab_spanner(
    label = "Total Population",
    columns = everything()
  ) |>
  fmt_integer()


# C) sza: The solar zenith angle is one measure of the solar position.

# Create a gt table based on a preprocessed `sza`
sza |>
  filter(latitude == 20) |>
  select(-latitude) |>
  filter(!is.na(sza)) |>
  pivot_wider(names_from = "tst", values_from = sza) |>
  gt(rowname_col = "month") |>
  sub_missing(missing_text = "") |>
  tab_stubhead(label = md("month<br>(20&deg;N)")) |>
  tab_header(title = md("&#x2600; Solar Zenith Angles &#x2600;")) |>
  tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
  )


# D) gtcars: The gtcars dataset takes off where mtcars left off.

# Create a gt table based on a preprocessed `gtcars`

gtcars |>
  filter(ctry_origin == "Germany") |>
  group_by(mfr) |>
  top_n(n = 2, msrp) |>
  ungroup() |>
  select(mfr, model, drivetrain, msrp) |>
  gt() |>
  tab_header(title = "Select German Automobiles") |>
  cols_merge(columns = c(mfr, model)) |>
  text_transform(
    locations = cells_body(columns = drivetrain),
    fn = function(x) toupper(x)
  ) |>
  fmt_currency(decimals = 0) |>
  cols_label(
    mfr = "Car",
    drivetrain = "Drivetrain",
    msrp = "MSRP"
  ) |>
  tab_footnote(
    footnote = "Prices in USD.",
    locations = cells_column_labels(columns = msrp)
  ) |>
  tab_footnote(
    footnote = "AWD = All Wheel Drive, RWD = Rear Wheel Drive.",
    locations = cells_column_labels(columns = drivetrain)
  ) |>
  opt_footnote_marks(marks = "letters")


# E) sp500: The S&P 500 is a capitalization-weighted index of about 500 leading companies

# Define the start and end dates for the data range
start_date <- "2010-06-02"
end_date <- "2010-06-15"

# The HTML decimal references for the black
# up- and down-pointing triangles are: #9650 and #9660;
# use an in-line style to apply color
up_arrow <- "<span style=\"color:green\">&#9650;</span>"
down_arrow <- "<span style=\"color:red\">&#9660;</span>"

# Create a gt table based on a preprocessed `sp500`
sp500 |>
  filter(date >= start_date & date <= end_date) |>
  select(-adj_close) |>
  gt() |>
  tab_header(
    title = "S&P 500",
    subtitle = glue::glue("{start_date} to {end_date}")
  ) |>
  fmt_date(
    columns = date,
    date_style = "day_m_year"
  ) |>
  fmt_currency(columns = c(open, high, low, close)) |>
  fmt_number(columns = volume, suffixing = TRUE) |>
  text_transform(
    locations = cells_body(
      columns = close,
      rows = close > open
    ),
    fn = function(x) paste(x, up_arrow)
  ) |>
  text_transform(
    locations = cells_body(
      columns = close,
      rows = close < open
    ),
    fn = function(x) paste(x, down_arrow)
  ) |>
  cols_label_with(
    columns = everything(),
    fn = ~ paste0(toupper(substr(., 1, 1)), substr(., 2, nchar(.)))
  )

# F) pizzaplace: The pizzaplace dataset

# Create a gt table based on a preprocessed `pizzaplace`
pizzaplace |>
  dplyr::group_by(type, size) |>
  dplyr::summarize(
    sold = n(),
    income = sum(price),
    .groups = "drop_last"
  ) |>
  gt(rowname_col = "size") |>
  tab_header(title = "Pizzas Sold in 2015") |>
  fmt_integer(columns = sold) |>
  fmt_currency(columns = income) |>
  summary_rows(
    columns = sold,
    fns = list(TOTAL = "sum"),
    fmt = list(~ fmt_integer(.))
  ) |>
  summary_rows(
    columns = income,
    fns = list(TOTAL = "sum"),
    fmt = list(~ fmt_currency(.))
  ) |>
  tab_options(
    summary_row.background.color = "#ACEACE",
    row_group.background.color = "#FFEFDB"
  )


# G) exibble: The example tibble that’s useful for gt is called exibble.

# Create a gt table based on `exibble`
exibble |>
  gt(
    rowname_col = "row",
    groupname_col = "group"
  ) |>
  fmt_number(
    columns = num,
    decimals = 2
  ) |>
  fmt_date(
    columns = date,
    date_style = "m_day_year"
  ) |>
  fmt_time(
    columns = time,
    time_style = "h_m_p"
  ) |>
  fmt_datetime(
    columns = datetime,
    date_style = "m_day_year",
    time_style = "h_m_p"
  ) |>
  fmt_currency(
    columns = currency,
    currency = "EUR"
  ) |>
  tab_options(
    column_labels.font.size = "small",
    table.font.size = "small",
    row_group.font.size = "small",
    data_row.padding = px(3)
  )

