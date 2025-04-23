# tinytable
# tinytable is a small but powerful R package to draw beautiful tables in a 
# variety of formats: HTML, LaTeX, Word, PDF, PNG, Markdown, and Typst
# Website: https://vincentarelbundock.github.io/tinytable/


# Install packages and load libraries

install.packages("tinytable")
library(tinytable)
library(webshot2) # tinytable requires the webshot2 package

# Basic table

x <- mtcars[1:5, 1:5]

tt(x)

# Complex table

cap <- "A simple {tinytable} example."
not <- " Cool table"

tt_1 <- tt(x, 
   caption = cap, 
   notes = not, 
   width = .5) |> 
  style_tt(
    i = 1:3,
    j = 1:2,
    background = "teal",
    color = "white",
    bold = TRUE) |>
  style_tt(
    i = 3:5,
    j = 4:5,
    background = "skyblue",
    color = "white",
    bold = TRUE) |>
  group_tt(
    j = list("Halloumi" = 1:2, "Tofu" = 4:5))

tt_1 

save_tt(tt_1, "tinytable/tiny_table.png")

