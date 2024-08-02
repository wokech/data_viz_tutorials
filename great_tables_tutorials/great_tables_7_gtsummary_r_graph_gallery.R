# Create summary and presentation ready tables with gtsummary

# Link: https://r-graph-gallery.com/package/gtsummary.html

# A) Install Packages

# install.packages("gtsummary")

# B) Examples

# Basic usage

library(palmerpenguins)
data(package = 'palmerpenguins')
library(gtsummary)

penguins %>%
  tbl_summary()

# Key features

#### Regression model results

# load dataset
data(Titanic)
df = as.data.frame(Titanic)

# load library
library(gtsummary)

# create the model
model = glm(Survived ~ Age + Class + Sex + Freq, family=binomial, data=df)

# generate table 
model %>%
  tbl_regression() %>% # regression summary function
  add_global_p() %>% # add p-values
  bold_labels() %>% # make label in bold
  italicize_levels() # make categories in label in italic

#### Summarize table

# load dataset and filter to keep just a few columns
data(mtcars) 
mtcars = mtcars %>%
  select(vs, mpg, drat, hp, gear)

# create summary table
mtcars %>%
  tbl_summary(
    by=vs, # group by the `vs` variable (dichotomous: 0 or 1)
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", # will display: mean (standard deviation)
      all_categorical() ~ "{n} / {N} ({p}%)" # will display: n / N (percentage)
    )
  ) %>%
  add_overall() %>% # statistics for all observations
  add_p() %>% # add p-values
  bold_labels() %>% # make label in bold
  italicize_levels() # make categories in label in italic


# Custom style of the table

data(iris)
library(gtsummary)
library(gt)

iris %>%
  tbl_summary(by=Species) %>%
  add_overall() %>% # info ignoring the `by` argument
  add_n() %>% # number of observations
  modify_header(label ~ "**Variables from the dataset**") %>% # title of the variables
  modify_spanning_header(c("stat_0", "stat_1", "stat_2", "stat_3") ~ "*Descriptive statistics of the iris flowers*, grouped by Species") %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*The iris dataset is probably the **most famous** dataset in the world*"))

#### Automate the creation of a summary table with gtsummary

# Default output for summary table

# create dataset
data("Titanic")
df = as.data.frame(Titanic)

# create the table
df %>%
  tbl_summary()


# Add p-values and statistical details

library(gtsummary)

# create dataset
data("Titanic")
df = as.data.frame(Titanic)

# create the table
df %>%
  tbl_summary(by=Survived) %>%
  add_overall() %>%
  add_p() #%>%

# Add a column based on a custom function

# create dataset
data("iris")
df = as.data.frame(iris)

my_anova = function(data, variable, by, ...) {
  result = aov(as.formula(paste(variable, "~", by)), data = data)
  summary(result)[[1]]$'Pr(>F)'[1] # Extracting the p-value for the group effect
}

# create the table
df %>%
  tbl_summary(by=Species) %>%
  add_overall() %>%
  add_p() %>%
  add_stat(fns = everything() ~ my_anova) %>%
  modify_header(
    list(
      add_stat_1 ~ "**p-value**",
      all_stat_cols() ~ "**{level}**"
    )
  ) %>%
  modify_footnote(
    add_stat_1 ~ "ANOVA")

#### Display results of a regression model in a table

# Default output for regression results

# create dataset
data("Titanic")
df = as.data.frame(Titanic)

# create the model
model = glm(Survived ~ Age + Class + Sex + Freq,
            family=binomial, data=df)

# generate table 
model %>%
  tbl_regression()


# Add more statistical details


# create dataset
data("Titanic")
df = as.data.frame(Titanic)

# create the model
model = glm(Survived ~ Age + Class + Sex + Freq,
            family=binomial, data=df)

# generate table 
model %>%
  tbl_regression(intercept=TRUE, conf.level=0.9) %>%
  add_glance_source_note() %>%
  add_global_p() %>%
  add_q() 


# Display results of different models

library(survival)
library(gtsummary)

data(trial)

model_reglog = glm(response ~ trt + grade, data=trial, family = binomial) %>% tbl_regression()
model_cox = coxph(Surv(ttdeath, death) ~ trt + grade, data=trial) %>% tbl_regression()

tbl_merge(
  list(model_reglog, model_cox),
  tab_spanner = c("**Tumor Response**", "**Time to Death**")
)
