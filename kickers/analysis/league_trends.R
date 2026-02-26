# Loading Dependencies ----------------------------------------------------

rm(list = ls())

packages = c(
  'tidyverse',
  'broom',
  'here',
  'nflreadr',
  'plotly',
  'DescTools'
)

check_packages = function(packages, repos = 'https://cloud.r-project.org') {
  invisible(
    lapply(packages, function(p) {
      #' Checking if the package is installed
      if (!requireNamespace(p, quietly = TRUE)) {
        message(paste('Package', p, 'not found. Attempting to install...'))
        
        #' Installing the missing package
        install.packages(p, repos = repos)
      }
      
      #' Loading the package
      suppressPackageStartupMessages(
        library(p, character.only = TRUE)
      )
    })
  )
}
check_packages(packages)

# -------------------------------------------------------------------------
# Loading Helper Functions ------------------------------------------------

helpers_file = here('kickers', 'helpers', 'helpers.R')
source(helpers_file)

# -------------------------------------------------------------------------
# Loading Data ------------------------------------------------------------

pbp_file = here('kickers', 'data', 'pp', 'pbp', 'pbp_pp.rdata')
load(pbp_file)

# -------------------------------------------------------------------------
# Determining FGA Threshold per Season ------------------------------------

#' K-Means Breakpoints
set.seed(1)

kmeans_data = pbp_pp %>%
  arrange(season, kicker_player_id, week, game_id, play_id) %>%
  group_by(season, kicker_player_id) %>%
  summarise(fg_att = last(season_fg_attempt), .groups = 'drop') %>%
  dplyr::select(season, fg_att) %>%
  group_split(season)
names(kmeans_data) = sort(unique(pbp_pp$season))

kmeans_list = map(
  kmeans_data,
  kmeans_threshold,
  v = 'fg_att',
  n = 2,
  iter = 50
)

kmeans_thresholds = enframe(
  kmeans_list,
  name = 'season',
  value = 'kmeans_threshold'
) %>%
  unnest(cols = c(kmeans_threshold)) %>%
  mutate(season = as.integer(season))

#' 25th Quantile Breakpoints
q25_data = pbp_pp %>%
  arrange(season, kicker_player_id, week, game_id, play_id) %>%
  group_by(season, kicker_player_id) %>%
  summarise(fg_att = last(season_fg_attempt), .groups = 'drop') %>%
  dplyr::select(season, fg_att)

q25_range = 1:25

q25_grid = expand_grid(season = sort(
  unique(q25_data$season)),
  threshold = q25_range
)

q25_thresholds = q25_grid %>%
  left_join(y = q25_data, by = 'season', relationship = 'many-to-many') %>%
  filter(fg_att >= threshold) %>%
  group_by(season, threshold) %>%
  summarise(
    q25_fga = quantile(fg_att, prob = .25, na.rm = TRUE),
    n_kickers = n(),
    .groups = 'drop'
  ) %>%
  group_by(season) %>%
  arrange(threshold, .by_group = TRUE) %>%
  mutate(
    q25_diff = q25_fga - lag(q25_fga),
    pct_change = (q25_diff / lag(q25_fga)) * 100
  ) %>%
  ungroup()

min_pct_change = 7

q25_thresholds = q25_thresholds %>%
  group_by(season) %>%
  arrange(threshold) %>%
  filter(pct_change > min_pct_change | lead(pct_change) > 5) %>%
  slice(1:2) %>%
  mutate(threshold_fga = mean(q25_fga, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(season, threshold_fga) %>%
  distinct() %>%
  rename(q25_threshold = 'threshold_fga')

#' Breakpoints Comparison
all_thresholds = inner_join(
  x = kmeans_thresholds,
  y = q25_thresholds,
  by = 'season'
)

# -------------------------------------------------------------------------
# Applying Season FGA Thresholds and Calculating FG% by Distance ----------

kicker_trends = pbp_pp %>%
  arrange(season, kicker_player_id, week, game_id, play_id) %>%
  mutate(
    fga_0_29 = if_else(kick_distance < 30, 1, 0),
    fga_30_39 = if_else(kick_distance >= 30 & kick_distance < 40, 1, 0),
    fga_40_49 = if_else(kick_distance >= 40 & kick_distance < 50, 1, 0),
    fga_50_59 = if_else(kick_distance >= 50 & kick_distance < 60, 1, 0),
    fga_60_ = if_else(kick_distance >= 60, 1, 0),
    
    fgm_0_29 = if_else(fga_0_29 == 1 & fg_result_clean == 'made', 1, 0),
    fgm_30_39 = if_else(fga_30_39 == 1 & fg_result_clean == 'made', 1, 0),
    fgm_40_49 = if_else(fga_40_49 == 1 & fg_result_clean == 'made', 1, 0),
    fgm_50_59 = if_else(fga_50_59 == 1 & fg_result_clean == 'made', 1, 0),
    fgm_60_ = if_else(fga_60_ == 1 & fg_result_clean == 'made', 1, 0)
  ) %>%
  group_by(season, kicker_name_clean, kicker_player_id) %>%
  summarise(
    age = last(age),
    weight = last(weight),
    height = last(height),
    
    total_fga = last(season_fg_attempt),
    fga_0_29 = sum(fga_0_29, na.rm = TRUE),
    fga_30_39 = sum(fga_30_39, na.rm = TRUE),
    fga_40_49 = sum(fga_40_49, na.rm = TRUE),
    fga_50_59 = sum(fga_50_59, na.rm = TRUE),
    fga_60_ = sum(fga_60_, na.rm = TRUE),
    
    total_fgm = sum(fg_result_clean == 'made', na.rm = TRUE),
    fgm_0_29 = sum(fgm_0_29, na.rm = TRUE),
    fgm_30_39 = sum(fgm_30_39, na.rm = TRUE),
    fgm_40_49 = sum(fgm_40_49, na.rm = TRUE),
    fgm_50_59 = sum(fgm_50_59, na.rm = TRUE),
    fgm_60_ = sum(fgm_60_, na.rm = TRUE),
    
    fg_pct = last(season_fg_pct),
    fg_pct_0_29 = if_else(fga_0_29 != 0, fgm_0_29 / fga_0_29, 0),
    fg_pct_30_39 = if_else(fga_30_39 != 0, fgm_30_39 / fga_30_39, 0),
    fg_pct_40_49 = if_else(fga_40_49 != 0, fgm_40_49 / fga_40_49, 0),
    fg_pct_50_59 = if_else(fga_50_59 != 0, fgm_50_59 / fga_50_59, 0),
    fg_pct_60_ = if_else(fga_60_ != 0, fgm_60_ / fga_60_, 0),
    
    .groups = 'drop'
  ) %>%
  left_join(y = kmeans_thresholds, by = 'season') %>%
  group_by(season) %>%
  filter(total_fga >= kmeans_threshold) %>%
  ungroup()

# -------------------------------------------------------------------------
# Calculating League FG% by Distance --------------------------------------

median_kick_dst = pbp_pp %>%
  semi_join(y = kicker_trends, by = join_by(season, kicker_player_id)) %>%
  group_by(season) %>%
  summarise(
    median_kick_dst = median(kick_distance, na.rm = TRUE),
    .groups = 'drop'
  )

league_trends = kicker_trends %>%
  group_by(season) %>%
  summarise(
    median_age = median(age, na.rm = TRUE),
    median_height = median(height, na.rm = TRUE),
    median_weight = median(weight, na.rm = TRUE),
    
    median_fga = median(total_fga, na.rm = TRUE),
    
    fga_0_29 = sum(fga_0_29, na.rm = TRUE),
    fga_30_39 = sum(fga_30_39, na.rm = TRUE),
    fga_40_49 = sum(fga_40_49, na.rm = TRUE),
    fga_50_59 = sum(fga_50_59, na.rm = TRUE),
    fga_60_ = sum(fga_60_, na.rm = TRUE),
    
    fgm_0_29 = sum(fgm_0_29, na.rm = TRUE),
    fgm_30_39 = sum(fgm_30_39, na.rm = TRUE),
    fgm_40_49 = sum(fgm_40_49, na.rm = TRUE),
    fgm_50_59 = sum(fgm_50_59, na.rm = TRUE),
    fgm_60_ = sum(fgm_60_, na.rm = TRUE),
    
    fg_pct_0_29 = if_else(fga_0_29 != 0, fgm_0_29 / fga_0_29, 0),
    fg_pct_30_39 = if_else(fga_30_39 != 0, fgm_30_39 / fga_30_39, 0),
    fg_pct_40_49 = if_else(fga_40_49 != 0, fgm_40_49 / fga_40_49, 0),
    fg_pct_50_59 = if_else(fga_50_59 != 0, fgm_50_59 / fga_50_59, 0),
    fg_pct_60_ = if_else(fga_60_ != 0, fgm_60_ / fga_60_, 0),
    
    .groups = 'drop'
  ) %>%
  left_join(y = median_kick_dst, by = 'season')

# -------------------------------------------------------------------------
# Saving the Analytic Data ------------------------------------------------

kicker_trends_file = here(
  'kickers',
  'data',
  'analytic',
  'kicker_trends',
  'kicker_trends.rdata'
)

league_trends_file = here(
  'kickers',
  'data',
  'analytic',
  'league_trends',
  'league_trends.rdata'
)

if (file.exists(kicker_trends_file)) {
  file.remove(kicker_trends_file)
  cat(basename(kicker_trends_file), 'has been deleted.\n')
} else {
  cat(basename(kicker_trends_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(kicker_trends, file = kicker_trends_file)
if (file.exists(kicker_trends_file)) {
  cat(basename(kicker_trends_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(kicker_trends_file), '\n')
}

if (file.exists(league_trends_file)) {
  file.remove(league_trends_file)
  cat(basename(league_trends_file), 'has been deleted.\n')
} else {
  cat(basename(league_trends_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(league_trends, file = league_trends_file)
if (file.exists(league_trends_file)) {
  cat(basename(league_trends_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(league_trends_file), '\n')
}

# -------------------------------------------------------------------------