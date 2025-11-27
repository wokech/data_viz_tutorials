# Part 1: Let’s Get Apply’ing

# https://drmowinckels.io/blog/2025/apply-multidimensional/lets-get-applying

# Learn to replace loops with vectorized apply functions in R for efficient 
# data manipulation. This guide covers converting loops to functions 
# using sapply and mapply, enhancing your programming skills by making code 
# more readable and less error-prone.

# Looptee loopy loop.

mtcars <- mtcars[, c("cyl", "mpg")]

for(i in 1:nrow(mtcars)){
  # Number of cylinders for this cars
  cylinders <- mtcars$cyl[i]
  
  # subset data to only cars with that cylinder
  tmp <- subset(mtcars, cyl == cylinders)
  
  # mean of mpg for this cylinder type
  mcyl <- mean(tmp$mpg)
  
  mtcars$mpg_dm[i] <- mtcars$mpg[i] - mcyl
}

# Look at the results
mtcars

# Use apply instead

# Example

# We apply over the mtcars dataset, in a 
# column-wise fashion (MARGIN = 2 , margin 1 is over rows), and apply 
# the function as.character on these vectors.

cars <- apply(mtcars, 
              MARGIN = 2, 
              FUN = as.character)
cars

as.data.frame(cars)

# If one used a LOOP

cars2 <- mtcars
for(i in seq_along(mtcars)){
  cars2[[i]] <- as.character(mtcars[[i]])
}
cars2

# When we create a function, the object name is the function’s name, and we use 
# the function() function to construct it. The arguments of your function, are 
# added in the function call, and what your function will do with these two 
# arguments, is specified in the following section within the curly braces.

demean <- function(i, data){
  # Number of cylinders for this cars
  cylinders <- data$cyl[i]
  
  # subset data to only cars with that cylinder
  tmp <- subset(data, cyl == cylinders)
  
  # mean of mpg for this cylinder type
  mcyl <- mean(tmp$mpg)
  
  return(data$mpg[i] - mcyl)
}

demean(1, mtcars)
demean(10, mtcars)
demean(13, mtcars)

# We can then apply this to all the elements in an apply

apply(1:nrow(mtcars), demean) # We get an error! why? Because apply() expects an data.frame, and a MARGIN specification.

sapply(1:nrow(mtcars), demean) # Its still erroring. This time we have another error.

# Correction

sapply(1:nrow(mtcars), demean, data = mtcars)

mtcars$dm <- sapply(1:nrow(mtcars), demean, data = mtcars) # assign the vector straight into mtcars

mtcars

# Use a subsample of mtcars

cars <- mtcars[1:20, ]
cars
dim(cars)

cars$dm_sub <- sapply(1:nrow(cars), demean, data = cars)
cars

# New function description

demean <- function(x, cylinders, data){
  # subset data to only cars with that cylinder
  tmp <- subset(data, cyl == cylinders)
  
  # mean of mpg for this cylinder type
  mcyl <- mean(tmp$mpg)
  
  return(x - mcyl)
}

demean(x = 21, cylinders = 6, data = mtcars)

demean(10.4, 4, mtcars)

sapply(c(21, 10.4), demean, data = mtcars, cylinders = 6)

# Use mapply to be more efficient

# The first x goes with the first cylinders, the second x with the second 
# cylinders and so on. But we have a single data source. This is provided 
# as a named list to the MoreArgs argument.

mapply(demean,
       x = mtcars$mpg,
       cylinders = mtcars$cyl,
       MoreArgs = list(
         data = mtcars
       ))

mtcars

# Try the tidyverse to avoid all this work above

library(tidyverse)

mtcars |> 
  group_by(cyl) |> 
  mutate(dm_t = mpg - mean(mpg))

# Part 2: Reading in multiple files without loops
# https://drmowinckels.io/blog/2024/lapply-files/

#' Function to generate stimuli
#' @param stimuli string vector
#' @param sign either 1 or -1, depending if the associated
#'    values should be positive or negative
#' @return named vector of values
generate_stimuli <- function(stimuli, sign = 1){
  # make sure sign either 1 or 1
  if(!sign %in% c(1, -1)){
    stop("argument 'sign' can only be 1 or -1.", call. = FALSE)
  }
  setNames(1:length(stimuli), stimuli) * sign
}

generate_stimuli(c("triangle", "diamond", "circle", "square"))

generate_stimuli(c("red", "blue", "green", "orange"), sign = -1)

########

#' Function to generate random data
#' @params n_rows how many rows the data should have
generate_data <- function(n_rows) {
  shapes <- generate_stimuli(
    c("square", "triangle", "circle", "rhombus", "diamond", "squiggle"),
    sign = 1
  )
  colours <- generate_stimuli(
    c("red", "blue", "green", "yellow", "purple", "orange"),
    sign = -1
  )
  
  # Initiate data
  data <- data.frame(
    trial  = 1:n_rows,
    shape  = sample(names(shapes), n_rows, replace = TRUE),
    colour = sample(names(colours), n_rows, replace = TRUE)
  )
  
  # Get corresponding value of the stimuli
  data$shape_value  <- shapes[match(data$shape, names(shapes))]
  data$colour_value <- colours[match(data$colour, names(colours))]
  
  # Get total value of stimuli
  data$value  <- data$shape_value + data$colour_value
  
  # Generate mock response
  data$choice <- ifelse(0 < rnorm(data$value, mean = 1, sd = 2), "accept", "reject")
  data$rt <- ceiling(rgamma(n_rows, shape = 2, scale = 600) + 300 )
  
  # Calculate accuracy based on value and choice
  data$accuracy <- dplyr::case_when(
    data$value == 0 ~ 1,
    data$value > 0 & data$choice == "accept" ~ 1,
    data$value < 0 & data$choice == "reject" ~ 1,
    TRUE ~ 0
  )
  
  data
}

#' Generate files with data
#' @params i number to append to file name
generate_files <- function(i){
  # Generate a random number of rows between 75 and 100
  num_rows <- sample(75:100, 1)
  
  # Generate the data
  data <- generate_data(num_rows)
  
  # Construct the file name
  file_name <- sprintf(
    "%s/data_%02d.csv",
    here::here("data_wrangling/datasets"),
    i
  )
  
  # Write the data to a CSV file, silently
  invisible(write.csv(data, file = file_name, row.names = FALSE))
}

# iterate through sequence of file numbers to generate the files
sapply(1:5, generate_files)

files <- list.files(here::here("data_wrangling/datasets"), full.names = TRUE)
files

# The loopy version

# We want to combine the data row-wise, meaning we get a really tall dataset.

data <- read.csv(files[[1]], nrows=1)
data <- data[0, ]
data

# Read in all files
for(file in files){
  # Read in the file
  tmp <- read.csv(file)
  # Add file name as src column
  tmp$src <- basename(file)
  # Bind rows together
  data <- rbind(data, tmp)
}

# Check thow the data look
str(data)

# The apply version

data <- lapply(files, read.csv)

# Inspect what the data object contains - 5 lists!
str(data)

# Combine
data <- do.call(rbind, data)
str(data)


data <- lapply(files, function(x){
  dt <- read.csv(x)
  dt$src <- basename(x)
  dt
})
data <- do.call(rbind, data)
str(data)

# The really pretty version

library(readr)
library(dplyr)

data <- read_csv(files, id = "src") |>
  mutate(src = basename(src))

str(data)

library(ggplot2)
data |>
  ggplot() +
  geom_density(aes(x = rt, group = src, colour = src)) +
  scale_colour_viridis_d() +
  theme_minimal()

# Part 3: Mapply: When You Need to Iterate Over Multiple Inputs
# https://drmowinckels.io/blog/2025/mapply/

# The key difference: sapply works with one varying input, mapply works with 
# multiple varying inputs that need to be paired up.

sample_sizes <- c(10, 20, 15, 30)
samples <- sapply(
  sample_sizes,
  function(n) rnorm(n, mean = 0, sd = 1),
  simplify = FALSE
)

# Look at the lengths
sapply(samples, length)

# If we have two variables, this won't work

sample_sizes <- c(10, 20, 15, 30)
means <- c(5, 10, 7, 12)

# This won't work as intended
samples <- sapply(sample_sizes, function(n) rnorm(n, mean = 0, sd = 1))
sapply(samples, mean)

# Enter mapply

# mapply is designed exactly for this situation - when you need to 
# pair up multiple vectors element-wise:

generate_samples <- function(n, mean_val) {
  rnorm(n, mean = mean_val, sd = 1)
}

sample_sizes <- c(10, 20, 15, 30)
means <- c(5, 10, 7, 12)

samples <- mapply(
  generate_samples,
  n = sample_sizes,
  mean_val = means,
  SIMPLIFY = FALSE
)

# Check the means of our samples
sapply(samples, mean)

# Only 2 means for 4 sizes
short_means <- c(5, 10)
samples <- mapply(
  generate_samples,
  n = sample_sizes,
  mean_val = short_means,
  SIMPLIFY = FALSE
)
sapply(samples, mean)


if (length(sample_sizes) != length(means)) {
  stop("sample_sizes and means must be the same length")
}

# A more complex example: Scaling data

# Create some test data
set.seed(42)
class_a <- rnorm(25, mean = 75, sd = 10)
class_b <- rnorm(30, mean = 82, sd = 8)
class_c <- rnorm(20, mean = 78, sd = 12)

scores <- list(class_a, class_b, class_c)
class_names <- c("Math", "Science", "English")
scores

sapply(scores, mean)
sapply(scores, sd)

# Math=80, Science=85, English=75
target_means <- c(80, 85, 75)

# Different spreads for each subject
target_sds <- c(5, 8, 10)

# Scaling function

rescale_scores <- function(scores, target_mean, target_sd) {
  # Standardize to mean=0, sd=1
  standardized <- (scores - mean(scores)) / sd(scores)
  standardized * target_sd + target_mean
}

# Loop Version

rescaled_scores <- list()
for (i in seq_along(scores)) {
  rescaled_scores[[i]] <- rescale_scores(
    scores[[i]],
    target_means[i],
    target_sds[i]
  )
}

# Apply Version

rescaled_scores <- mapply(
  rescale_scores, # the function you want to apply
  scores = scores, # The varying inputs
  target_mean = target_means,
  target_sd = target_sds,
  SIMPLIFY = FALSE # to keep the output as a list
)

# Check our results
sapply(rescaled_scores, mean)

sapply(rescaled_scores, sd)

# Adding more arguments

rescale_and_label <- function(scores, target_mean, target_sd, class_name) {
  rescaled <- rescale_scores(scores, target_mean, target_sd)
  data.frame(
    score = rescaled,
    class = class_name,
    student_id = seq_along(rescaled)
  )
}

result_data <- mapply(
  rescale_and_label,
  scores = scores,
  target_mean = target_means,
  target_sd = target_sds,
  class_name = class_names,
  SIMPLIFY = FALSE
)

# Combine into one data.frame
final_data <- do.call(rbind, result_data)
head(final_data)

# When you have some constant arguments

rescale_with_bounds <- function(
    scores,
    target_mean,
    target_sd,
    min_score,
    max_score
) {
  rescaled <- rescale_scores(scores, target_mean, target_sd)
  # Apply bounds
  pmax(min_score, pmin(max_score, rescaled))
}

# All classes have same score bounds
bounded_scores <- mapply(
  rescale_with_bounds,
  scores = scores,
  target_mean = target_means,
  target_sd = target_sds,
  MoreArgs = list(
    min_score = 0,
    max_score = 100
  ),
  SIMPLIFY = FALSE
)

# Check that we don't exceed bounds
sapply(bounded_scores, function(x) c(min = min(x), max = max(x)))

# The mapply pattern

# I find mapply most useful for:
  
# 1) Simulation studies - varying multiple parameters simultaneously
# 2) Data processing - when different groups need different treatments
# 3) Modeling - fitting the same model type with different parameters

# The pattern is always:
  
# 1) Write a function that takes multiple arguments
# 2) Create vectors for each varying argument (same length)
# 3) Use mapply to pair them up
# 4) Add any constant arguments via MoreArgs

# Tidyverse equivalent

# For completeness, the purrr equivalent uses pmap, which stands for “parallel map”. 
# A map is a function that applies a function to each element of a list or vector, 
# so acts like lapply or sapply. 

library(purrr)

result_data <- list(
  scores = scores,
  target_mean = target_means,
  target_sd = target_sds,
  class_name = class_names
) |>
  pmap(rescale_and_label)

result_data

# Part 4: Mastering Apply: From Matrices to Multidimensional Neuroimaging Data - Dr. Mowinckel’s

# https://drmowinckels.io/blog/2025/apply-multidimensional/

# Deep dive into apply() for matrices and high-dimensional arrays. From basic 
# row/column operations to complex fMRI analyses with 5D data. Learn to master 
# the MARGIN parameter for neuroimaging and scientific computing.

# apply() is actually quite different from sapply() and mapply(). While those 
# work on lists and vectors, apply() is designed specifically for 
# matrices and arrays.

# apply(X, MARGIN, FUN, ...) for matrix or array data is best

# X is your matrix or array / MARGIN specifies which dimension(s) 
# to preserve (1 = rows, 2 = columns, 3+ = higher dimensions) / FUN is 
# the function to apply / ... are additional arguments passed to FUN

# The MARGIN parameter tells apply() which dimension to “collapse” by applying your function

# The MARGIN parameter can be either numeric (e.g., 1 for rows, 2 for columns) or a 
# character vector specifying the names of the dimensions 

# Examples

# Create a 4x3 matrix (4 rows, 3 columns)
test_matrix <- matrix(1:12, nrow = 4, ncol = 3)
test_matrix

# MARGIN = 1: apply across rows (each row becomes one value)
apply(test_matrix, MARGIN = 1, FUN = sum)

# MARGIN = 2: apply across columns (each column becomes one value)
apply(test_matrix, MARGIN = 2, FUN = sum)

# Here’s how I think about it:

# MARGIN = 1: “Process each row” → Number of rows determines output length
# MARGIN = 2: “Process each column” → Number of columns determines output length
# The margin you specify is the dimension that gets preserved in your output. 
# Everything else gets collapsed by your function.

# Real-world example: Student grades

grades <- matrix(
  c(
    85, 92, 78, 88,  # Student 1
    90, 85, 95, 87,  # Student 2
    78, 83, 80, 85,  # Student 3
    95, 88, 92, 90,  # Student 4
    82, 79, 84, 81   # Student 5
  ),
  nrow = 5,
  byrow = TRUE
)

colnames(grades) <- c("Quiz1", "Quiz2", "Midterm", "Final")
rownames(grades) <- paste("Student", 1:5)
grades

# Student averages (across columns for each row)
student_averages <- apply(grades, MARGIN = 1, FUN = mean)
student_averages

dimnames(grades)

dimnames(grades) <- list(
  Student = rownames(grades),
  Assignment = colnames(grades)
)
dimnames(grades)

apply(grades, MARGIN = "Student", FUN = mean)

# Assignment averages (across rows for each column)
assignment_averages <- apply(grades, MARGIN = 2, FUN = mean)
assignment_averages

# Beyond mean: useful functions for apply

# Standard deviation for each student (higher = more variable)
apply(grades, 1, sd)

# Range
apply(grades, 2, range)

# which assignment was each student’s worst
apply(grades, 1, which.min)


# which assignment was each student’s best
apply(grades, 1, which.max)

# Get the assignment names instead of indices
colnames(grades)[apply(grades, 1, which.min)]
colnames(grades)[apply(grades, 1, which.max)]

# Custom functions with apply
# Calculate letter grades based on each student’s average

get_letter_grade <- function(scores) {
  avg <- mean(scores)
  if (avg >= 90) {
    return("A")
  }
  if (avg >= 80) {
    return("B")
  }
  if (avg >= 70) {
    return("C")
  }
  if (avg >= 60) {
    return("D")
  }
  return("F")
}

# Letter grade for each student
apply(grades, 1, get_letter_grade)


# Figure out if students are improving over time

calculate_trend <- function(scores) {
  # Simple linear trend (positive = improving, negative = declining)
  x <- seq_along(scores)
  trend <- cor(x, scores)
  return(trend)
}

# Trend for each student (are they improving over time?)
trends <- apply(grades, 1, calculate_trend)
trends

# Working with 3D arrays

# 3D array: 5 students × 4 assignments × 3 classes
set.seed(42)
scores_3d <- array(
  round(rnorm(60, mean = 85, sd = 10)),
  dim = c(5, 4, 3),
  dimnames = list(
    Student = paste("S", 1:5, sep = ""),
    Assignment = c("Quiz1", "Quiz2", "Midterm", "Final"),
    Class = c("Math", "Science", "English")
  )
)

# Look at the whole structure
scores_3d


# Just the Math class
scores_3d[,, "Math"]


# Student averages across all assignments and classes
# Keep dimension 1, collapse dimensions 2 and 3
apply(scores_3d, MARGIN = "Student", FUN = mean)


# Assignment difficulty across all students and classes
# Keep dimension 2, collapse dimensions 1 and 3
apply(scores_3d, MARGIN = "Assignment", FUN = mean)


# Class averages across all students and assignments
# Keep dimension 3, collapse dimensions 1 and 2
apply(scores_3d, MARGIN = "Class", FUN = mean)


# Average for each student in each class
# Keep dimensions 1 and 3, collapse dimension 2 (assignments)
apply(scores_3d, MARGIN = c(1, 3), FUN = mean)


# The key insight is that MARGIN tells you what dimensions you want to preserve 
# in your output. Everything else gets fed to your function.

# Real neuroimaging example: fMRI data

# Neuroimaging data is inherently multidimensional. Most of us are aware that 
# digital images are made of lots and lots of pixels — tiny squares, each with 
# a single value. Together, all these pixels form a complete 2D image.

# MRI - Voxels - The brain scan is a cube made up of many tiny cubes (voxels)

# MRI data usually contains values for three spatial dimensions: 
# X (left-right), Y (anterior-posterior), and Z (inferior-superior). 
# So a standard MRI scan is 3D: width, height, depth, a larger cube 
# made of tiny cubes (voxels).

# An fMRI scan is often called “4D” because it captures three spatial 
# dimensions (x, y, z) plus time. Each 3D brain volume is like a 
# snapshot, and the scanner takes many of these snapshots in sequence—typically 
# one every 2–3 seconds. When we combine these 3D volumes over time, we 
# get a time series: a movie of brain activity, or, in data terms, 
# an array of cubes changing over time.

library(oro.nifti)

# Get data - https://openneuro.org/datasets/ds005038/versions/1.0.3

# Path to our BIDS dataset
bids_dir <- here::here("data_wrangling/datasets")

# Load a functional run for subject 01
func_file <- file.path(
  bids_dir,
  "sub-101_ses-pre_task-DT_run-01_bold.nii.gz"
)

fmri_data <- readNIfTI(func_file, reorient = FALSE)
dim(fmri_data)

# This is a 4D array: 64 voxels in x, 64 in y, 37 in z, and 133 timepoints. 
# That’s 64x64x37x133 = 298 individual numbers. You definitely don’t want to 
# write nested loops for this!

# Calculate mean activation across time for each voxel
# Keep x, y, z dimensions; collapse time (dimension 4)
mean_activation <- apply(
  fmri_data,
  MARGIN = c(1, 2, 3),
  FUN = mean
)
dim(mean_activation)


# Visualize a slice of the mean activation
image(
  mean_activation[,,20], # Slice at z=20
  col = viridis::viridis(256),
  main = "Mean Activation - Subject 01 (Slice z=20)",
  xlab = "X-axis",
  ylab = "Y-axis"
)


# Calculate temporal standard deviation for each voxel
temporal_sd <- apply(
  fmri_data,
  MARGIN = c(1, 2, 3),
  FUN = sd
)

# Coefficient of variation (CV) - normalized measure of variability
cv <- temporal_sd / mean_activation


# Visualize a slice of the CV
image(
  cv[,,20], # Slice at z=20
  col = viridis::viridis(256),
  main = "Coefficient of Variation - Subject 01 (Slice z=20)",
  xlab = "X-axis",
  ylab = "Y-axis"
)


# Subject-level summary: mean signal across all voxels over time
# Keep time dimension; collapse spatial dimensions (1, 2, 3)
global_signal <- apply(
  fmri_data,
  MARGIN = 4,
  FUN = mean
)


# Plot the global signal
library(ggplot2)

data.frame(
  time = 1:length(global_signal),
  signal = global_signal
) |>
  ggplot(aes(x = time, y = signal)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  labs(
    title = "Global Signal - Subject 01",
    x = "Time (TRs)",
    y = "Mean Signal"
  ) +
  theme_minimal() 



# Multiple subjects - WORK ON THIS WHEN YOU GET MORE DATA!!!



