# The gt Table package

# Articles

# Case Study: Clinical Tables
# Website:  https://gt.rstudio.com/articles/case-study-clinical-tables.html

# A) Install package and load library

# install.packages("gt")
library(gt)
library(tidyverse)

# The gt package contains the rx_adsl dataset, which resembles 
# the structure of a common ADSL ADaM dataset for 
# clinical trial data.

rx_adsl |> str()

# B) Demographic Summary Tables

custom_summary <- function(df, group_var, sum_var) {
  
  group_var <- rlang::ensym(group_var)
  sum_var <- rlang::ensym(sum_var)
  
  is_categorical <- 
    is.character(eval(expr(`$`(df, !!sum_var)))) |
    is.factor(eval(expr(`$`(df, !!sum_var)))) 
  
  if (is_categorical) {
    
    category_lbl <- 
      sprintf("%s, n (%%)", attr(eval(expr(`$`(df, !!sum_var))), "label"))
    
    df_out <-
      df |>
      dplyr::group_by(!!group_var)  |> 
      dplyr::mutate(N = dplyr::n()) |> 
      dplyr::ungroup() |> 
      dplyr::group_by(!!group_var, !!sum_var) |> 
      dplyr::summarize(
        val = dplyr::n(),
        pct = dplyr::n()/mean(N),
        .groups = "drop"
      ) |> 
      tidyr::pivot_wider(
        id_cols = !!sum_var, names_from = !!group_var,
        values_from = c(val, pct)
      ) |> 
      dplyr::rename(label = !!sum_var) |> 
      dplyr::mutate(
        across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
        category = category_lbl
      )
    
  } else {
    
    category_lbl <-
      sprintf(
        "%s (%s)",
        attr(eval(expr(`$`(df, !!sum_var))), "label"),
        attr(eval(expr(`$`(df, !!sum_var))), "units")
      )
    
    df_out <- 
      df |> 
      dplyr::group_by(!!group_var) |> 
      dplyr::summarize(
        n = sum(!is.na(!!sum_var)),
        mean = mean(!!sum_var, na.rm = TRUE),
        sd = sd(!!sum_var, na.rm = TRUE),
        median = median(!!sum_var, na.rm = TRUE),
        min = min(!!sum_var, na.rm = TRUE),
        max = max(!!sum_var, na.rm = TRUE),
        min_max = NA,
        .groups = "drop"
      ) |> 
      tidyr::pivot_longer(
        cols = c(n, mean, median, min_max),
        names_to = "label",
        values_to = "val"
      ) |> 
      dplyr::mutate(
        sd = ifelse(label == "mean", sd, NA),
        max = ifelse(label == "min_max", max, NA),
        min = ifelse(label == "min_max", min, NA),
        label = dplyr::recode(
          label,
          "mean" = "Mean (SD)",
          "min_max" = "Min - Max",
          "median" = "Median"
        )
      ) |> 
      tidyr::pivot_wider(
        id_cols = label,
        names_from = !!group_var,
        values_from = c(val, sd, min, max)
      ) |> 
      dplyr::mutate(category = category_lbl)
  }
  
  return(df_out)
}

adsl_summary <- 
  dplyr::filter(rx_adsl, ITTFL == "Y") |> 
  (\(data) purrr::map_df(
    .x = dplyr::vars(AGE, AAGEGR1, SEX, ETHNIC, BLBMI),
    .f = \(x) custom_summary(df = data, group_var = TRTA, sum_var = !!x)
  ))()

rx_adsl_tbl <- 
  adsl_summary |> 
  gt(
    rowname_col = "label",
    groupname_col = "category"
  ) |> 
  tab_header(
    title = "x.x: Demographic Characteristics",
    subtitle = "x.x.x: Demographic Characteristics - ITT Analysis Set"
  )

rx_adsl_tbl

# Format the numbers

rx_adsl_tbl <- 
  rx_adsl_tbl |> 
  fmt_integer(
    columns = starts_with(c("val_", "min_", "max_")),
    rows = label %in% c("n", "Median", "Min - Max")
  ) |> 
  fmt_percent(
    columns = starts_with("pct_"),
    decimals = 1
  ) |> 
  fmt_number(
    columns = starts_with("val_"),
    rows = label == "Mean (SD)",
    decimals = 1
  ) |> 
  fmt_number(
    columns = starts_with("sd_"),
    rows = label == "Mean (SD)",
    decimals = 2
  ) 

rx_adsl_tbl

# Merge columns

rx_adsl_tbl <- 
  rx_adsl_tbl |> 
  cols_merge(
    columns = c("val_Placebo", "pct_Placebo", "sd_Placebo", "min_Placebo", "max_Placebo"),
    pattern = "<<{1}>><< ({2})>><< ({3})>><<{4} - {5}>>"
  ) |> 
  cols_merge(
    columns = c("val_Drug 1", "pct_Drug 1", "sd_Drug 1", "min_Drug 1", "max_Drug 1"),
    pattern = "<<{1}>><< ({2})>><< ({3})>><<{4} - {5}>>"
  )

rx_adsl_tbl

# Alignment

rx_adsl_tbl <-
  rx_adsl_tbl |> 
  tab_stub_indent(
    rows = everything(),
    indent = 5
  ) |> 
  opt_align_table_header(align = "left") 

rx_adsl_tbl

# Fix column width and alignment

rx_adsl_tbl <-
  rx_adsl_tbl |> 
  cols_width(
    starts_with("val_") ~ px(200),
    1 ~ px(250)
  ) |> 
  cols_align(
    align = "center",
    columns = starts_with("val_")
  )

rx_adsl_tbl

# Fix column labels

### Count subjects per arm and summarize values in a list
arm_n <-
  rx_adsl |> 
  dplyr::filter(ITTFL == "Y") |> 
  dplyr::group_by(TRTA) |> 
  dplyr::summarize(
    lbl = sprintf("%s N=%i (100%%)", unique(TRTA), dplyr::n()),
    .groups = "drop"
  ) |> 
  dplyr::arrange(TRTA)

collbl_list <- as.list(arm_n$lbl)
names(collbl_list) <- paste0("val_", arm_n$TRTA)

rx_adsl_tbl <- 
  rx_adsl_tbl |> 
  cols_label(.list = collbl_list)

rx_adsl_tbl


# C) Response / Event Rate Analysis Tables

rx_responders <- 
  rx_adsl |> 
  dplyr::filter(ITTFL == "Y") |> 
  dplyr::group_by(TRTA, AAGEGR1) |> 
  dplyr::summarize(
    n_resp = sum(EVNTFL == "Y"),
    n_total = dplyr::n(),
    pct = 100 * sum(EVNTFL == "Y") / dplyr::n(),
    ci_up = 100 * (
      1 + (dplyr::n() - sum(EVNTFL == "Y")) / (
        (sum(EVNTFL == "Y") + 1) * qf(
          0.975,
          2 * (sum(EVNTFL == "Y") + 1),
          2 * (dplyr::n() - sum(EVNTFL == "Y"))
        )
      )
    )^(-1),
    ci_low = ifelse(
      sum(EVNTFL == "Y") == 0,
      0,
      100 * (
        1 + (dplyr::n() - sum(EVNTFL == "Y") + 1) /
          (sum(EVNTFL == "Y") * qf(
            0.025,
            2 * sum(EVNTFL == "Y"),
            2 * (dplyr::n() - sum(EVNTFL == "Y") + 1)
          )
          )
      )^(-1)
    ),
    odds = sum(EVNTFL == "Y") / (dplyr::n() - sum(EVNTFL == "Y")),
    .groups = "drop"
  ) |> 
  tidyr::pivot_wider(
    id_cols = AAGEGR1,
    names_from = TRTA,
    values_from = c(n_resp, n_total, pct, ci_up, ci_low, odds)
  ) |> 
  dplyr::mutate(
    or = ifelse(
      odds_Placebo == 0,
      NA_real_,
      !! sym("odds_Drug 1") / odds_Placebo
    ),
    or_ci_low = exp(
      log(or) - qnorm(0.975) * sqrt(
        1 / n_resp_Placebo +
          1 / !!sym("n_resp_Drug 1") + 
          1 / (n_total_Placebo - n_resp_Placebo) + 
          1 / (!!sym("n_total_Drug 1") - !!sym("n_resp_Drug 1"))
      )
    ),
    or_ci_up = exp(
      log(or) + qnorm(0.975) * sqrt(
        1 / n_resp_Placebo + 
          1 / !!sym("n_resp_Drug 1") +
          1 / (n_total_Placebo - n_resp_Placebo) +
          1 / (!!sym("n_total_Drug 1") - !!sym("n_resp_Drug 1"))
      )
    )
  ) |> 
  dplyr::select(-tidyselect::starts_with("odds_"))

# Let’s first create a basic gt table with a left-aligned table title and subtitle.

rx_resp_tbl <- rx_responders |> 
  gt() |> 
  tab_header(
    title = "x.x: Efficacy Data",
    subtitle = "x.x.x: Occurence of Event per Subgroup - {gt} Analysis Set"
  ) |> 
  opt_align_table_header(align = "left")

rx_resp_tbl

# Format columns

rx_resp_tbl <- 
  rx_resp_tbl |> 
  fmt_integer(columns = starts_with("n_")) |> 
  fmt_number(columns = starts_with(c("pct_", "ci_")), decimals = 1) |> 
  fmt_number(columns = starts_with("or"), decimals = 2) 

rx_resp_tbl

# Merge Columns

rx_resp_tbl <-
  rx_resp_tbl |> 
  cols_merge(
    columns = c("n_resp_Placebo", "n_total_Placebo", "pct_Placebo"),
    pattern = "{1}/{2} ({3})"
  ) |> 
  cols_merge(
    columns = c("n_resp_Drug 1", "n_total_Drug 1", "pct_Drug 1"),
    pattern = "{1}/{2} ({3})"
  ) |> 
  cols_merge(
    columns = c("ci_low_Placebo", "ci_up_Placebo"),
    pattern = "[{1}, {2}]"
  ) |> 
  cols_merge(
    columns = c("ci_low_Drug 1", "ci_up_Drug 1"),
    pattern = "[{1}, {2}]"
  ) |> 
  cols_merge(
    columns = c("or_ci_low", "or_ci_up"),
    pattern = "[{1}, {2}]"
  ) |> 
  tab_spanner(
    label = "Drug 1",
    columns = c("n_resp_Drug 1", "ci_low_Drug 1")
  ) |> 
  tab_spanner(
    label = "Placebo",
    columns = c("n_resp_Placebo", "ci_low_Placebo")
  ) 

rx_resp_tbl

# Label row groups

rx_resp_tbl <-
  rx_resp_tbl |> 
  tab_row_group(
    label = "Age",
    rows = everything()
  ) 

rx_resp_tbl

# Column widths and alignment

rx_resp_tbl <- 
  rx_resp_tbl |> 
  cols_align(
    align = "center",
    columns = starts_with(c("n_", "ci", "or"))
  ) |> 
  cols_label(
    .list = c(
      "AAGEGR1" = "",
      "n_resp_Placebo" = "Event Rate (%)",
      "ci_low_Placebo" = "[95% CI]",
      "n_resp_Drug 1" = "Event Rate (%)",
      "ci_low_Drug 1" = "[95% CI]",
      "or" = "Odds ratio",
      "or_ci_low" = "[95% CI]"
    )
  ) |> 
  cols_width(
    1 ~ px(80),
    everything() ~ px(120)
  ) |> 
  cols_align(align = "left", columns = 1) 

rx_resp_tbl

# Include footnotes

rx_resp_tbl <-
  rx_resp_tbl |> 
  tab_footnote(
    footnote = "Event rate 95% exact confidence interval uses the Clopper−Pearson method.",
    locations = cells_column_labels(
      columns = c("ci_low_Placebo", "ci_low_Drug 1")
    ),
    placement = "right"
  ) |> 
  tab_options(footnotes.marks = letters)

rx_resp_tbl

# D) Protocol Deviation Table

# For the summary table for protocol deviations (PDs) we will use a second 
# CDISC-flavored dataset, namely rx_addv

rx_addv |> str()

# Summary

addv_sum <- 
  rx_addv |> 
  dplyr::group_by(TRTA) |> 
  dplyr::mutate(
    NTOT = n_distinct(USUBJID),
    .groups = "drop"
  ) |> 
  dplyr::group_by(TRTA, PARCAT1, PARAM, CRIT1FL) |> 
  dplyr::summarize(
    n = sum(AVAL, na.rm = TRUE),
    pct = 100 * sum(AVAL, na.rm = TRUE) / mean(NTOT),
    .groups = "drop"
  ) |> 
  tidyr::pivot_wider(
    id_cols = c(PARCAT1, PARAM),
    names_from = c(TRTA, CRIT1FL),
    values_from = c(n, pct)
  ) |> 
  dplyr::mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) |> 
  dplyr::add_row(PARAM = "Subjects with at least:", .before = 1)

addv_sum

# Alignment

addv_tbl <- 
  addv_sum |> 
  gt(rowname_col = "PARAM") |> 
  tab_header(
    title = "xx.x: Demographic and Baseline Data",
    subtitle = "xx.x.x: Major Protocol Deviations and Relationship to COVID-19 - ITT Set"
  ) |> 
  opt_align_table_header(align = "left")

addv_tbl

# Create row groups and summarize the rows

addv_tbl <- 
  addv_tbl |> 
  tab_row_group(
    label = " ",
    rows = PARCAT1 == "PROTOCOL DEVIATION"
  ) |> 
  row_group_order(groups = c(NA, " ")) |>
  summary_rows(
    groups = " ",
    columns = where(is.numeric),
    fns = list(label = "Study Procedure Deviations", fn = "sum"),
    side = "top"
  )

addv_tbl

# Hide columns

addv_tbl <- 
  addv_tbl |> 
  cols_hide(columns = "PARCAT1")

addv_tbl

# Format and merge columns

addv_tbl <- 
  addv_tbl |> 
  sub_missing(
    rows = 1,
    missing_text = ""
  ) |> 
  fmt_number(
    columns = starts_with("pct"),
    decimals = 1
  ) |> 
  cols_merge_n_pct(
    col_n = "n_Placebo_Y",
    col_pct = "pct_Placebo_Y"
  ) |> 
  cols_merge_n_pct(
    col_n = "n_Placebo_N",
    col_pct = "pct_Placebo_N"
  ) |> 
  cols_merge_n_pct(
    col_n = "n_Drug 1_Y",
    col_pct = "pct_Drug 1_Y"
  ) |> 
  cols_merge_n_pct(
    col_n = "n_Drug 1_N",
    col_pct = "pct_Drug 1_N"
  )

addv_tbl

# Modify columns and create column spanners

addv_tbl <- 
  addv_tbl |> 
  tab_spanner(
    label = md("COVID-19 Related"),
    columns = c("n_Placebo_Y", "n_Placebo_N"),
    id = "cov_pla"
  ) |> 
  tab_spanner(
    label = md("COVID-19 Related"),
    columns = c("n_Drug 1_Y", "n_Drug 1_N"),
    id = "cov_dru"
  ) |> 
  tab_spanner(
    label = md("Placebo  \n  N=90 (100%)  \n   n (%)"),
    columns = c("n_Placebo_Y", "n_Placebo_N")
  ) |> 
  tab_spanner(
    label = md("Drug 1  \n  N=90 (100%)  \n   n (%)"),
    columns = c("n_Drug 1_Y", "n_Drug 1_N")
  ) |> 
  cols_label(
    .list = list(
      "n_Placebo_Y" = "Yes",
      "n_Placebo_N" = "No",
      "n_Drug 1_Y" = "Yes",
      "n_Drug 1_N" = "No"
    )
  ) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_spanners(spanners = everything())
  )

addv_tbl

# We can now add a footnote, indicating that subjects can have 
# more than one PD during the course of the study.

addv_tbl <- 
  addv_tbl |> 
  tab_footnote(
    footnote = "Subjects can have more than one Protocol Deviation throughout the study.",
    locations = cells_stub(rows = c("At least one major Protocol Deviation")),
    placement = "right"
  )

addv_tbl

# Style the table

addv_tbl |> 
  cols_align(
    align = "center",
    columns = 3:6
  ) |> 
  cols_align(
    align = "left",
    columns = 1:2
  ) |> 
  tab_stub_indent(
    rows = PARCAT1 == "PROTOCOL DEVIATION",
    indent = 5
  )




