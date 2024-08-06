# Title

# https://koenderks.github.io/rcityviews/
# https://www.sercanazizoglu.com.tr/en/blog/how-to-create-a-city-map-in-rstudio-with-rcityviews/

## To get this to work use options(download.file.method = "wininet") and not
## > options(download.file.method = "libcurl")

#install.packages("remotes")
#devtools::install_dev("remotes")

#devtools::install_github("koenderks/rcityviews", dependencies = TRUE)
library(rcityviews)

cityview_shiny()

