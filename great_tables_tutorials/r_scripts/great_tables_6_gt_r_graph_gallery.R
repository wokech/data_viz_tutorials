# Create beautiful tables with gt

# Website: https://r-graph-gallery.com/package/gt.html

# A) Install package and load library

# install.packages("gt")
library(gt)
library(tidyverse)

# B) Examples

#### Create a simple data frame
data = data.frame(
  Country = c("USA", "China", "India", "Brazil"),
  Capitals = c("Washington D.C.", "Beijing", "New Delhi", "Brasília"),
  Population = c(331, 1441, 1393, 212),
  GDP = c(21.43, 14.34, 2.87, 1.49)
)

# Alternatively you can do (same output):
#gt(data)

# Default

# Use the gt function
data %>%
  gt()

# Title and Subtitle

data %>%
  gt() %>%
  tab_header(title = md("What a **nice title**"),
             subtitle = md("Pretty *cool subtitle* too, `isn't it?`"))


# Footer and source

data %>%
  gt() %>%
  tab_footnote(
    footnote = "Source: James et al., 2020",
    locations = cells_body(columns = Country, rows = 3)
  )

# Sub-header

data %>%
  gt() %>%
  tab_spanner(
    label = "Number",
    columns = c(GDP, Population)) %>%
  tab_spanner(
    label = "Label",
    columns = c(Country, Capitals)
  )

#### Customize your titles in your tables with gt table

# Add a title in markdown

# create and display the gt table 
data %>%
  gt() %>%
  tab_header(title = md("Some **title**"))

# Add a title in HTML

# create and display the gt table 
data %>%
  gt() %>%
  tab_header(title = html("<span style='color:red;'>A red title</span>"))

# Combine title and subtitle

# create and display the gt table 
data %>%
  gt() %>%
  tab_header(title = html("<span style='color:red;'>A <strong>red</strong> title</span>"),
             subtitle = md("This text will be *below the title* and is written in `markdown`"))

# More complex example

library(htmltools)

# create and display the gt table 
data %>%
  gt() %>%
  tab_header(title = html("<span style='color:red;'>A <strong>red</strong> title</span>"),
             subtitle = tagList(
               tags$div(style = css(`text-align` = "center"),
                        HTML(web_image("https://www.r-project.org/logo/Rlogo.png")
                        )
               )
             )
  )

#### Customize your footer in your gt tables

# dataset
data2 = data.frame(
  Planet = c("Earth", "Mars", "Jupiter", "Venus"),
  Moons = c(1, 2, 79, 0),
  Distance_from_Sun = c(149.6, 227.9, 778.3, 108.2),
  Diameter = c(12742, 6779, 139822, 12104)
)

# create and display the gt table (equivalent to "gt(data)")
data2 %>%
  gt()

# Add a simple footer in markdown

data2 %>%
  gt() %>%
  tab_footnote(footnote = md("This text is the footer of this **table**"))

# Footer with reference

# create and display the gt table 
data2 %>%
  gt() %>%
  tab_footnote(footnote = md("Measured in **millions** of Km"),
               locations = cells_column_labels(columns = Distance_from_Sun))

# Footer with multiple references

# create and display the gt table 
data2 %>%
  gt() %>%
  tab_footnote(footnote = md("Measured in **millions** of Km"),
               locations = cells_column_labels(columns = Distance_from_Sun)) %>%
  tab_footnote(footnote = md("Measured in **Km**"),
               locations = cells_column_labels(columns = Diameter))

# Change the type of element that indicates the reference

# create and display the gt table 
data2 %>%
  gt() %>%
  tab_footnote(footnote = md("Measured in **millions** of Km"),
               locations = cells_column_labels(columns = Distance_from_Sun)) %>%
  tab_footnote(footnote = md("Measured in **Km**"),
               locations = cells_column_labels(columns = Diameter)) %>%
  tab_footnote(footnote = md("The original data are from *Some Organization*")) %>%
  opt_footnote_marks(marks = "LETTERS")

