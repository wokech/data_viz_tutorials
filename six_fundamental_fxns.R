# The 6 Most Fundamental Functions for Data Cleaning with R
# Albert Rapp

# Website: https://albert-rapp.de/posts/24_data_cleaning_fundamentals/24_data_cleaning_fundamentals.html

# Libraries
library(tidyverse)

# Load penguins dataset
penguins <- palmerpenguins::penguins
penguins

# 1) COUNT

# Count all the things with count()
penguins |> 
  count()

# Count species
penguins |> 
  count(species)

# Count species and SORT
penguins |> 
  count(species, sort = TRUE)

# Count species, island, and SORT
penguins |> 
  count(species, island, sort = TRUE)

# 2) SELECT
# Get the right columns with select()

penguins |> 
  select(flipper_length_mm, bill_length_mm, species)

# 3) FILTER
# Reduce the rows to specific observations with filter()

penguins |> 
  filter(bill_length_mm > 55)

penguins |> 
  filter(between(bill_length_mm, 55, 58))

penguins$bill_length_mm
penguins$bill_length_mm > 55
which(penguins$bill_length_mm > 55)
between(c(54, 55, 56, 57, 58), 55, 57)

# 4) MUTATE
# Calculating and transforming columns with mutate()

penguins |> 
  mutate(bill_length_mm = scale(bill_length_mm)[,1]) |> 
  select(bill_length_mm)

penguins |> 
  mutate(scaled_bill_length = scale(bill_length_mm)[,1]) |> 
  select(bill_length_mm, scaled_bill_length)

penguins$bill_length_mm
scale(penguins$bill_length_mm)[,1]

# 5) SUMMARIZE
# Reduce your data to key quantities with summarize()

penguins |> 
  summarize(
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE)
  )

penguins |> 
  summarize(
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE),
    .by = species
  )

# 6) ARRANGE
# Sort your data with arrange()

penguins |> 
  summarize(
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE),
    .by = species
  ) |> 
  arrange(mean_flipper_length)

penguins |> 
  summarize(
    mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
    mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE),
    .by = species
  )  |> 
  arrange(desc(mean_flipper_length))

