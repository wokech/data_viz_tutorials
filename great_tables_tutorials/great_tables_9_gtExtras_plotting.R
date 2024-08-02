# Plotting with gtExtras

# Website: https://jthomasmock.github.io/gtExtras/articles/plotting-with-gtExtras.html

# A) Install packages

# install.packages("gtExtras")
library(gtExtras)
library(tidyverse)

# Sparklines

mtcars %>% 
  head()

# Use summarize(list_data = list(col_name))

car_summary <- mtcars %>%
  group_by(cyl) %>%
  summarize(
    mean = mean(mpg),
    sd = sd(mpg),
    # must end up with list of data for each row in the input dataframe
    mpg_data = list(mpg),
    .groups = "drop"
  )

car_summary

# Plot the table

car_summary %>%
  arrange(desc(cyl)) %>% 
  gt() %>%
  gt_plt_sparkline(mpg_data) %>%
  fmt_number(columns = mean:sd, decimals = 1)


# Sparkline alternatives

car_summary %>%
  arrange(desc(cyl)) %>% 
  gt() %>%
  gt_plt_dist(mpg_data, 
              type = "density", 
              line_color = "blue", 
              fill_color = "red") %>%
  fmt_number(columns = mean:sd, decimals = 1)


car_summary %>%
  arrange(desc(cyl)) %>% 
  gt() %>%
  gt_plt_dist(mpg_data, type = "histogram", line_color = "purple", 
                        fill_color = "green", bw = 4) %>%
  fmt_number(columns = mean:sd, decimals = 1)

# Inline bars

mtcars %>%
  select(cyl:wt, mpg) %>% 
  head() %>%
  gt() %>%
  gt_plt_bar(column = mpg, keep_column = TRUE, width = 35)

# Percent bars

mtcars %>%
  head() %>%
  select(cyl, mpg) %>%
  mutate(mpg_pct_max = round(mpg/max(mpg) * 100, digits = 2),
                mpg_scaled = mpg/max(mpg) * 100) %>%
  mutate(mpg_unscaled = mpg) %>%
  gt() %>%
  gt_plt_bar_pct(column = mpg_scaled, scaled = TRUE) %>%
  gt_plt_bar_pct(column = mpg_unscaled, scaled = FALSE,
                 fill = "blue", background = "lightblue") %>%
  cols_align("center", contains("scale")) %>%
  cols_width(4 ~ px(125),
             5 ~ px(125))


# Inline Win Loss plots

# Get the data 

# Use the nflreadr package

#install.packages("nflreadr")
library(nflreadr)

games_df <- nflreadr::load_schedules() %>%
  filter(season == 2020, game_type == "REG") %>%
  select(game_id, team_home = home_team, team_away = away_team, result, week) %>%
  tidyr::pivot_longer(contains('team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>%
  mutate(
    result = ifelse(home_away == 'home', result, -result),
    win = ifelse(result == 0 , 0.5, ifelse(result > 0, 1, 0))
  ) %>%
  select(week, team, win) %>%
  mutate(
    team = case_when(
      team == 'STL' ~ 'LA',
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      T ~ team
    )
  )

team_df <- nflreadr::load_teams() %>%
  select(team_wordmark, team_abbr, team_conf, team_division)

joined_df <- games_df %>%
  group_by(team) %>%
  summarise(
    Wins = length(win[win==1]),
    Losses = length(win[win==0]),
    outcomes = list(win), .groups = "drop") %>%
  left_join(team_df, by = c("team" = "team_abbr")) %>%
  select(team_wordmark, team_conf, team_division, Wins:outcomes)

final_df <- joined_df %>%
  filter(team_conf == "AFC") %>%
  group_by(team_division) %>%
  arrange(desc(Wins)) %>%
  ungroup() %>%
  arrange(team_division) %>%
  select(-team_conf) %>%
  mutate(team_division = stringr::str_remove(team_division, "AFC |NFC ")) %>%
  mutate(
    team_division = factor(team_division,
                           levels = c("North", "South", "East", "West")
    )
  ) %>%
  arrange(team_division)

# Plot the data

final_df %>%
  gt(groupname_col = "team_division") %>%
  cols_label(team_wordmark = "") %>%
  cols_align("left", team_division) %>%
  gt_plt_winloss(outcomes, max_wins = 16, type = "pill") %>%
  gt_img_rows(columns = team_wordmark, height = 20) %>%
  gt_theme_538() %>%
  tab_header(
    title = add_text_img(
      "2020 Results by Division",
      url = "https://github.com/nflverse/nflfastR-data/raw/master/AFC.png",
      height = 30
    )
  ) %>%
  tab_options(data_row.padding = px(2))


# Inline bar plots

gt_bar_plot_tab <- mtcars %>%
  head() %>%
  select(cyl, mpg) %>%
  mutate(
    mpg_pct_max = round(mpg / max(mpg) * 100, digits = 2),
    mpg_scaled = mpg / max(mpg) * 100
  ) %>%
  mutate(mpg_unscaled = mpg) %>%
  gt() %>%
  gt_plt_bar_pct(column = mpg_scaled, scaled = TRUE) %>%
  gt_plt_bar_pct(column = mpg_unscaled, scaled = FALSE, fill = "blue", background = "lightblue") %>%
  cols_align("center", contains("scale")) %>%
  cols_width(
    4 ~ px(125),
    5 ~ px(125)
  )

gt_bar_plot_tab


# Stacked Percent bar charts

player_df <- tibble(
  player = c(
    "Evan Mobley",
    "Sandro Mamukelashvili",
    "Charles Bassey",
    "Luke Garza",
    "Moses Wright",
    "Neemias Queta",
    "Isaiah Jackson",
    "Day'Ron Sharpe"
  ),
  team = c(
    "USC", "Seton Hall", "Western Kentucky",
    "Iowa", "Georgia Tech", "Utah St", "Kentucky",
    "North Carolina"
  ),
  ht = c(
    "7'0\"",
    "6'10\"",
    "6'10\"",
    "6'11\"",
    "6'9\"",
    "7'1\"",
    "6'11\"",
    "6'10\""
  ),
  dk_pct_time = c(40, 48, 50, 50, 51, 55, 60, 66),
  dk_pps = c(1.62, 1.02, 1.54,1.33,1.46,1.37,1.33,1.18),
  tip_pct_time = c(26, 10, 19, 15, 25, 27, 15, 24),
  tip_pps = c(0.88, .97,1,1.05, .63, .85, .76, .84),
  jmp_pct_time = c(33, 42, 31, 35, 25, 18, 25, 10),
  jmp_pps = c(.91, .91, .78, 1.04, .86, .74, .71, .42)
) %>%
  left_join(
    tibble(
      player = c(
        "Evan Mobley",
        "Sandro Mamukelashvili",
        "Charles Bassey",
        "Luke Garza",
        "Moses Wright",
        "Neemias Queta",
        "Isaiah Jackson",
        "Day'Ron Sharpe"
      ) %>% rep(each = 3),
      shot_type = c("Dunks + Lays", "Hooks + Floats", "Jumpers") %>% rep(8)
    ) %>%
      mutate(
        shot_type = factor(shot_type, levels = c("Jumpers", "Hooks + Floats", "Dunks + Lays")),
        shot_mix = c(
          40, 26, 33,
          48, 10, 42,
          50, 19, 31,
          50, 15, 35,
          51, 25, 25,
          55, 27, 18,
          60, 15, 25,
          66, 24, 10
        )
      ),
    by = "player"
  )


basic_tb <- player_df %>%
  group_by(player) %>%
  summarize(dunks = shot_mix[1], list_data = list(shot_mix)) %>%
  arrange(dunks) %>%
  gt()


basic_tb %>%
  gt_plt_bar_stack(list_data, width = 65,
                   labels = c("DUNKS", "HOOKS/FLOATS", "JUMPERS"),
                   palette= c("#ff4343", "#bfbfbf", "#0a1c2b")) %>%
  gt_theme_538()


# Bullet chart

set.seed(37)

bullet_df <- tibble::rownames_to_column(mtcars) %>%
  dplyr::select(rowname, cyl:drat, mpg) %>%
  dplyr::group_by(cyl) %>%
  dplyr::mutate(target_col = mean(mpg)) %>%
  dplyr::slice_sample(n = 3) %>%
  dplyr::ungroup() 

bullet_df %>%
  gt() %>%
  gt_plt_bullet(column = mpg, target = target_col, width = 45,
                palette = c("lightblue", "black"))

bullet_df %>%
  dplyr::mutate(plot_column = mpg) %>%
  gt() %>%
  gt_plt_bullet(column = plot_column, target = target_col, width = 45) %>%
  fmt_number(mpg, decimals = 1)

