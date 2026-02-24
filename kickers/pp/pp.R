#!/usr/bin/env Rscript --vanilla

# Loading Dependencies ----------------------------------------------------

packages = c('tidyverse', 'lubridate', 'here')

check_packages = function(packages, repos = 'https://cloud.r-project.org') {
  invisible(
    lapply(packages, function(p) {
      if (!requireNamespace(p, quietly = TRUE)) {
        message(paste('Package', p, 'not found. Attempting to install...'))
        install.packages(p, repos = repos)
      }
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

pbp_clean_file = here('kickers', 'data', 'clean', 'pbp', 'pbp_clean.rdata')
load(pbp_clean_file)

stats_clean_file = here('kickers', 'data', 'clean', 'stats', 'stats_clean.rdata')
load(stats_clean_file)

# -------------------------------------------------------------------------
# Creating the FG Dataset -------------------------------------------------

pbp_pp = pbp_clean %>%
  filter(field_goal_attempt == 1) %>%
  arrange(season, week, game_id) 

# -------------------------------------------------------------------------
# Calculating each Kicker's PBP Career Stats ------------------------------

pbp_pp = pbp_pp %>%
  group_by(kicker_player_id) %>%
  mutate(birth_date = ymd(birth_date),
         birth_year = year(birth_date),
         birth_month_day = format(birth_date, '%m-%d'),
         age = season - birth_year,
         season_end_date = ymd(paste0(season, '-02-26')),
         birth_date_season = ymd(paste0(season, '-', birth_month_day)),
         age = ifelse(birth_date_season <= season_end_date, age, age-1),
         
         career_fg_pct = cumsum(fg_result_clean == 'made')/
           cumsum(field_goal_attempt),
         
         career_fg_attempt = row_number(),
         
         lag_fg_result = lag(fg_result_clean, n = 1),
         
         b2b_fg_makes = if_else(
           (fg_result_clean == 'made') & (lag_fg_result == 'made'), 1, 0
         ),
         b2b_fg_makes = if_else(is.na(b2b_fg_makes), 0, b2b_fg_makes),
         
         b2b_fg_misses = if_else(
           (fg_result_clean == 'missed') & (lag_fg_result == 'missed'), 1, 0
         ),
         b2b_fg_misses = if_else(is.na(b2b_fg_misses), 0, b2b_fg_misses)) %>%
  ungroup()

pbp_pp = pbp_pp %>%
  group_by(kicker_player_id) %>%
  mutate(streak_id = data.table::rleid(fg_result_clean)) %>%
  ungroup() %>%
  group_by(kicker_player_id, streak_id) %>%
  mutate(
    streak_length = row_number(),
    seq_fg_makes = if_else(fg_result_clean == 'made', streak_length, 0),
    seq_fg_misses = if_else(fg_result_clean == 'missed', streak_length, 0)
  ) %>%
  ungroup()

# -------------------------------------------------------------------------
# Calculating each Kicker's PBP Seasonal Stats ----------------------------

pbp_pp = pbp_pp %>%
  group_by(kicker_player_id, season) %>%
  mutate(
    season_fg_pct = cumsum(fg_result_clean == 'made')/
           cumsum(field_goal_attempt),
    season_fg_attempt = row_number()) %>%
  ungroup()

# -------------------------------------------------------------------------
# Selecting Variables for the PBP Data ------------------------------------

pbp_remove = c(
  'weather', 
  'drive_time_of_possession',
  'roof',
  'surface',
  'temp',
  'wind',
  'birth_year', 
  'birth_month_day', 
  'season_end_date',
  'birth_date_season'
)

pbp_pp = pbp_pp %>%
  dplyr::select(!all_of(pbp_remove))

# -------------------------------------------------------------------------
# Calculating Each Player's Age per Season --------------------------------

stats_remove = c(
  'birth_year', 
  'birth_month_day', 
  'season_end_date',
  'birth_date_season'
)

stats_pp = stats_clean %>%
  group_by(player_id) %>%
  mutate(
    birth_date = ymd(birth_date),
         birth_year = year(birth_date),
         birth_month_day = format(birth_date, '%m-%d'),
         age = season - birth_year,
         season_end_date = ymd(paste0(season, '-02-26')),
         birth_date_season = ymd(paste0(season, '-', birth_month_day)),
         age = ifelse(birth_date_season <= season_end_date, age, age-1)
  ) %>%
  dplyr::select(!all_of(stats_remove))

# -------------------------------------------------------------------------
# Saving the Preprocessed Data -------------------------------------------------

pbp_pp_file = here('kickers', 'data', 'pp', 'pbp', 'pbp_pp.rdata')
stats_pp_file = here('kickers', 'data', 'pp', 'stats', 'stats_pp.rdata')

#' Replacing the old pp pbp data with new data
if (file.exists(pbp_pp_file)) {
  file.remove(pbp_pp_file)
  cat(basename(pbp_pp_file), 'has been deleted.\n')
} else {
  cat(basename(pbp_pp_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(pbp_pp, file = pbp_pp_file)
if (file.exists(pbp_pp_file)) {
  cat(basename(pbp_pp_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(pbp_pp_file), '\n')
}

#' Replacing the old pp stats data with new data
if (file.exists(stats_pp_file)) {
  file.remove(stats_pp_file)
  cat(basename(stats_pp_file), 'has been deleted.\n')
} else {
  cat(basename(stats_pp_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(stats_pp, file = stats_pp_file)
if (file.exists(stats_pp_file)) {
  cat(basename(stats_pp_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(stats_pp_file), '\n')
}

# -------------------------------------------------------------------------