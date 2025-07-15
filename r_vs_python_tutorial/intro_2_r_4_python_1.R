# An introduction to Python for R users
####https://occasionaldivergences.com/posts/python-intro/####

# 1. Get started

# Import data

library(tidyverse)

customer_data <- read_csv(here::here("r_vs_python_tutorial", "data", "customer_data.csv"))
glimpse(customer_data)

# Filter observations

filter(customer_data, gender == "Female", income > 70000)

# Slice observations

slice(customer_data, 1:5)

# Sort observations

arrange(customer_data, desc(birth_year))

# Select variables

select(customer_data, region, review_text)

# Create new variables

mutate(customer_data, income = income / 1000)

# Join data frames

store_transactions <- read_csv(here::here("r_vs_python_tutorial", "data", "store_transactions.csv"))

left_join(customer_data, store_transactions, join_by(customer_id))

# Consecutive lines of code

customer_data |> 
  left_join(store_transactions, join_by(customer_id)) |> 
  filter(region == "West", feb_2005 == max(feb_2005)) |> 
  mutate(age = 2024 - birth_year) |> 
  select(age, feb_2005) |> 
  arrange(desc(age)) |> 
  slice(1)

# Summarize discrete data

customer_data |> 
  count(region, college_degree)

# Summarize continuous data

customer_data |>
  summarize(
    avg_income = mean(income),
    avg_credit = mean(credit)
  )

# Summarize discrete and continuous data

customer_data |>
  group_by(gender, region) |>
  summarize(
    n = n(),
    avg_income = mean(income),
    avg_credit = mean(credit)
  ) |> 
  arrange(desc(avg_income))

# Lazy evaluation

data_db <- customer_data |>
  group_by(gender, region) |>
  summarize(
    n = n(),
    avg_income = mean(income),
    avg_credit = mean(credit)
  ) |> 
  arrange(desc(avg_income))

data_db |> show_query()

data_db |> collect()

# 2. Visualization

# Column plots

customer_data |> 
  count(region) |> 
  ggplot(aes(x = region, y = n)) +
  geom_col()

customer_data |> 
  ggplot(aes(x = region)) +
  geom_bar()

customer_data |> 
  count(region, college_degree) |> 
  ggplot(aes(x = region, y = n, fill = college_degree)) +
  geom_col(position = "dodge")

# Histograms

customer_data |> 
  ggplot(aes(x = income)) +
  geom_histogram()

# Scatterplots

customer_data |> 
  ggplot(aes(x = income, y = credit)) +
  geom_point()

customer_data |> 
  ggplot(aes(x = star_rating, y = income)) +
  geom_jitter(size = 3, alpha = 0.5)

# Line plots

customer_data |> 
  ggplot(aes(x = review_time, y = star_rating)) +
  geom_line()

customer_data |> 
  drop_na(star_rating) |> 
  select(review_time, star_rating) |> 
  mutate(
    review_time = mdy(review_time),
    review_year = year(review_time)
  ) |> 
  group_by(review_year) |> 
  summarize(avg_star_rating = mean(star_rating)) |>
  ggplot(aes(x = review_year, y = avg_star_rating)) +
  geom_line()

# Density plots

customer_data |> 
  ggplot(aes(x = income, fill = gender)) +
  geom_density(alpha = 0.5)

# Facets

customer_data |> 
  count(region, college_degree, gender) |> 
  ggplot(aes(x = region, y = n, fill = college_degree)) +
  geom_col(position = "dodge") +
  facet_wrap(~ gender)

# 3. Modeling

# Prepare data

library(tidymodels)

# Set seed, variable, and parameter values.
set.seed(42)
nobs <- 500
beta0 <- -5
beta1 <- 5
beta2 <- 2
beta3 <- 0

# Simulate data.
sim_data <- tibble(
  x1 = round(runif(nobs, min = 0, max = 20)),
  x2 = rbinom(nobs, size = 2, prob = c(0.7, 0.3)) |> 
    as.factor() |> fct_recode("level01" = "0", "level02" = "1", "level03" = "2"),
  y = beta0 + beta1 * x1 + beta2 * ifelse(x2 == "level02", 1, 0) + beta3 * ifelse(x2 == "level03", 1, 0) + rnorm(nobs, mean = 0, sd = 3)
)

sim_data

# Training and testing split.
sim_split <- initial_split(sim_data, prop = 0.90)

# Feature engineering.
sim_recipe <- training(sim_split) |>
  recipe(y ~ .) |> 
  step_dummy(all_nominal_predictors())

# Specify and fit a model

# Model specification.
sim_lm <- linear_reg() |> 
  set_engine("lm")

# Compose a workflow.
sim_wf_lm <- workflow() |> 
  add_recipe(sim_recipe) |> 
  add_model(sim_lm)

# Fit the model.
sim_lm_fit <- fit(sim_wf_lm, data = training(sim_split))

# Evaluate model fit

# Visualize slope parameter estimates.
tidy(sim_lm_fit, conf.int = TRUE) |> 
  ggplot(aes(x = term)) + 
  geom_point(aes(y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  geom_hline(yintercept = 0, color = "red")

# Compute RMSE.
sim_lm_fit |> 
  predict(new_data = testing(sim_split)) |>
  bind_cols(testing(sim_split)) |>
  rmse(truth = y, estimate = .pred)
