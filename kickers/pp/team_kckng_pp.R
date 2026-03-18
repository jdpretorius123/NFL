# Loading Dependencies ----------------------------------------------------

rm(list = ls())

packages = c(
  'DescTools',
  'zoo',
  'data.table',
  'geosphere',
  'tidyverse',
  'here',
  'nflreadr',
  'plotly'
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

stadiums_url = paste0(
  'https://raw.githubusercontent.com', 
  '/greerreNFL',
  '/Stadiums',
  '/main',  
  '/data',
  '/stadiums.csv'
)
stadiums = read_csv(file = stadiums_url, show_col_types = FALSE)

team_stadiums_url = paste0(
  'https://raw.githubusercontent.com', 
  '/greerreNFL',
  '/Stadiums',
  '/main',  
  '/data',
  '/team_stadiums.csv'
)
team_stadiums = read_csv(file = team_stadiums_url, show_col_types = FALSE)

pbp_clean = here('kickers', 'data', 'clean', 'pbp', 'pbp_clean.rdata')
load(pbp_clean)

pbp_file = here('kickers', 'data', 'pp', 'pbp', 'pbp_pp.rdata')
load(pbp_file)

league_trends_file = here(
  'kickers',
  'data',
  'analytic',
  'league_trends',
  'league_trends.rdata'
)
load(league_trends_file)

kicker_trends_file = here(
  'kickers',
  'data',
  'analytic',
  'kicker_trends',
  'kicker_trends.rdata'
)
load(kicker_trends_file)

# -------------------------------------------------------------------------
# Determining the Starter Coverage per Season -----------------------------

nfl_total_fga = pbp_pp %>%
  group_by(season) %>%
  summarise(nfl_total_fga = n(), .groups = 'drop')

starter_coverage = league_trends %>%
  left_join(y = nfl_total_fga, by = join_by(season)) %>%
  mutate(
    total_fga = fga_0_29 + fga_30_39 + fga_40_49 + fga_50_59 + fga_60_,
    starter_coverage = (total_fga/nfl_total_fga) * 100
  )

# -------------------------------------------------------------------------
# Team Demos Dataset ------------------------------------------------------

#' Rows = 24,726; Cols = 9
demos_pbp = pbp_pp %>%
  arrange(season, posteam, week, game_id, play_id) %>%
  dplyr::select(
    season, game_id, week, play_id, posteam,
    kicker_player_id, age, height, weight
  ) %>%
  distinct() %>%
  rename(team = posteam)

#' Rows = 799; Cols = 6
team_demos = demos_pbp %>%
  group_by(season, team) %>%
  summarise(
    n_kickers = n_distinct(kicker_player_id),
    mean_age = mean(age, na.rm = TRUE),
    mean_weight = mean(weight, na.rm = TRUE),
    mean_height = mean(height, na.rm = TRUE),
    .groups = 'drop'
  )

# -------------------------------------------------------------------------
# Team Stats Dataset ------------------------------------------------------

stats_cols_to_keep = c(
  'season', 'game_id', 'week', 'play_id', 'posteam',
  'kicker_player_id',
  'goal_to_go', 'score_differential', 
  'fg_result_clean', 'kick_distance', 
  'career_fg_pct', 'career_fg_attempt', 
  'lag_fg_result',
  'b2b_fg_makes', 'b2b_fg_misses',
  'seq_fg_makes', 'seq_fg_misses',
  'season_fg_pct', 'season_fg_attempt'
)
stats_pbp = pbp_pp %>%
  dplyr::select(all_of(stats_cols_to_keep)) %>%
  arrange(season, posteam, week, game_id, play_id) %>%
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
  rename(team = posteam)

team_stats = stats_pbp %>%
  group_by(season, team) %>%
  summarise(
    med_kick_dst = median(kick_distance, na.rm = TRUE),
    med_score_diff = median(score_differential, na.rm = TRUE),
    goal_to_go = sum(goal_to_go, na.rm = TRUE),
    
    total_fga = n(),
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
    
    fg_pct = if_else(total_fga != 0, total_fgm / total_fga, 0),
    fg_pct_0_29 = if_else(fga_0_29 != 0, fgm_0_29 / fga_0_29, 0),
    fg_pct_30_39 = if_else(fga_30_39 != 0, fgm_30_39 / fga_30_39, 0),
    fg_pct_40_49 = if_else(fga_40_49 != 0, fgm_40_49 / fga_40_49, 0),
    fg_pct_50_59 = if_else(fga_50_59 != 0, fgm_50_59 / fga_50_59, 0),
    fg_pct_60_ = if_else(fga_60_ != 0, fgm_60_ / fga_60_, 0),
    
    .groups = 'drop'
  )

# -------------------------------------------------------------------------
# Environment Dataset -----------------------------------------------------

env_cols_to_keep = c(
  'season',
  'game_id',
  'week',
  'play_id',
  'posteam',
  'wind_clean',
  'temp_clean',
  'weather_clean',
  'surface_clean',
  'roof_clean',
  'start_time_clean'
)
env_pbp = pbp_pp %>%
  arrange(season, team = posteam, week, game_id, play_id) %>%
  dplyr::select(all_of(env_cols_to_keep)) %>%
  distinct() %>%
  rename(team = 'posteam')

team_env = env_pbp %>%
  group_by(season, team) %>%
  summarise(
    med_wind = median(wind_clean, na.rm = TRUE),
    med_temp = median(temp_clean, na.rm = TRUE),
    bad_wthr_gms = sum(weather_clean == 'bad', na.rm = TRUE) / n(),
    grass_flds = sum(surface_clean == 'grass', na.rm = TRUE) / n(),
    indoor_gms = sum(roof_clean == 'indoors', na.rm = TRUE) / n(),
    prime_time_gms = sum(start_time_clean == 'night', na.rm = TRUE) / n(),
    .groups = 'drop'
  )

# -------------------------------------------------------------------------
# Team Records ------------------------------------------------------------

records_pbp = pbp_clean %>%
  arrange(season, week, game_id, play_id) %>%
  dplyr::select(season, game_id, home_team, away_team, result) %>%
  group_by(season, game_id) %>%
  summarise(
    home_team = last(home_team),
    away_team = last(away_team),
    result = last(result),
    .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = 'status',
    values_to = 'team'
  ) %>%
  mutate(
    win = if_else(
      (result > 0 & status == 'home_team') | (result < 0 & status == 'away_team'),
      1, 0
    ),
    loss = if_else(
      (result < 0 & status == 'home_team') | (result > 0 & status == 'away_team'),
      1, 0
    ),
    tie = if_else(result == 0, 1, 0)
  ) %>%
  group_by(season, team) %>%
  mutate(
    wins = lag( cumsum(win), n = 1, default = 0),
    losses = lag( cumsum(loss), n = 1, default = 0),
    ties = lag( cumsum(tie), n = 1, default = 0),
    win_pct = if_else(
      (wins + losses + ties) == 0, 0,
      (wins + (0.5 * ties)) / (wins + losses + ties)
    ),
    gms_plyd = wins + losses + ties
  ) %>%
  ungroup()

opp_records = records_pbp %>%
  dplyr::select(game_id, team, win_pct, gms_plyd) %>%
  rename(
    opp_team = team,
    opp_win_pct = win_pct,
    opp_gms_plyd = gms_plyd
  )

records_pbp = records_pbp %>%
  left_join(y = opp_records, by = 'game_id', relationship = 'many-to-many') %>%
  filter(team != opp_team) %>%
  mutate(
    gt_500 = if_else(
      opp_gms_plyd > 0 & opp_win_pct > 0.5, 1, 0
    ),
    le_500 = if_else(
      opp_gms_plyd > 0 & opp_win_pct <= 0.5, 1, 0
    )
  ) %>%
  group_by(season, team) %>%
  mutate(
    b2b_le_500 = if_else(le_500 == 1 & lag(le_500, default = 0) == 1, 1, 0),
    seq_le_500 = sequence(rle(le_500)$lengths) * le_500,
    
    b2b_gt_500 = if_else(gt_500 == 1 & lag(gt_500, default = 0) == 1, 1, 0),
    seq_gt_500 = sequence(rle(gt_500)$lengths) * gt_500
  ) %>%
  ungroup()

team_records = records_pbp %>%
  group_by(season, team) %>%
  summarise(
    wins = sum(win, na.rm = TRUE),
    losses = sum(loss, na.rm = TRUE),
    ties = sum(tie, na.rm = TRUE),
    
    opp_le_500 = sum(le_500, na.rm = TRUE),
    b2b_opp_le_500 = sum(b2b_le_500, na.rm = TRUE),
    max_seq_opp_le_500 = max(seq_le_500, na.rm = TRUE),
    
    opp_gt_500 = sum(gt_500, na.rm = TRUE),
    b2b_opp_gt_500 = sum(b2b_gt_500, na.rm = TRUE),
    max_seq_opp_gt_500 = max(seq_gt_500, na.rm = TRUE),
    .groups = 'drop'
  )

# -------------------------------------------------------------------------
# Team Schedule Dataset ---------------------------------------------------

stadium_info = stadiums %>%
  dplyr::select(
    stadium_id,
    lat, lon, altitude, 
    tz_offset
  ) %>%
  distinct()

options(nflreadr.verbose = FALSE)
schedules = load_schedules(2001:2025)
schedules_clean = schedules %>%
  filter(game_type == 'REG') %>%
  mutate(
    gameday = as.Date(gameday),
    away_team = case_when(
      away_team == 'SD' ~ 'LAC',
      away_team == 'OAK' ~ 'LV',
      away_team == 'STL' ~ 'LA',
      .default = away_team
    ),
    home_team = case_when(
      home_team == 'SD' ~ 'LAC',
      home_team == 'OAK' ~ 'LV',
      home_team == 'STL' ~ 'LA',
      .default = home_team
    )
  ) %>%
  dplyr::select(
    game_id, 
    season, week, gameday, weekday, gametime, stadium_id,
    away_team, home_team, 
    away_rest, home_rest,
    div_game
  ) 

games_home_team_info = schedules_clean %>%
  left_join(y = stadium_info, by = join_by(stadium_id)) %>%
  rename(
    home_lat = lat,
    home_lon = lon,
    home_alt = altitude,
    home_tz_offset = tz_offset
  )

home_stadiums = schedules_clean %>%
  dplyr::select(
    team = home_team,
    home_gameday = gameday,
    home_stadium_id = stadium_id
  ) %>%
  arrange(home_gameday, team)

temp_schedules_pbp = games_home_team_info %>%
  left_join(
    y = home_stadiums,
    by = join_by(away_team == team, closest(gameday >= home_gameday))
  ) %>%
  left_join(
    y = home_stadiums %>%
      rename(future_gameday = home_gameday, future_stadium_id = home_stadium_id),
    by = join_by(away_team == team, closest(gameday <= future_gameday))
  ) %>%
  mutate(away_stadium_id = coalesce(home_stadium_id, future_stadium_id)) %>%
  dplyr::select(
    -home_gameday, -home_stadium_id, -future_gameday, -future_stadium_id
  ) %>%
  left_join(y = stadium_info, by = join_by(away_stadium_id == stadium_id)) %>%
  rename(
    away_lat = lat,
    away_lon = lon,
    away_alt = altitude,
    away_tz_offset = tz_offset
  ) 

temp_schedules_pbp = temp_schedules_pbp %>%
  mutate(
    travel_dist_meters = distHaversine(
      cbind(away_lon, away_lat),
      cbind(home_lon, home_lat)
    ),
    
    alt_chg = home_alt - away_alt,
    alt_chg_desc = factor(
      case_when(
        alt_chg > 0 ~ 'increase',
        alt_chg < 0 ~ 'decrease',
        alt_chg == 0 ~ 'no_change'
      )
    ),
    tz_chg = home_tz_offset - away_tz_offset,
    tz_chg_desc = factor(
      case_when(
        tz_chg > 0 ~ 'forward',
        tz_chg < 0 ~ 'backward',
        tz_chg == 0 ~ 'no_change'
      )
    )
  )

schedules_pbp = temp_schedules_pbp %>%
  arrange(season, week, game_id, weekday, gametime) %>%
  pivot_longer(
    cols = c(away_team, home_team),
    names_to = 'status',
    values_to = 'team'
  ) %>%
  dplyr::select(
    game_id, season, week,
    team, status, stadium_id,
    away_rest, home_rest, 
    travel_dist_meters,
    alt_chg, alt_chg_desc, 
    tz_chg, tz_chg_desc
  )

team_schedules = schedules_pbp %>%
  group_by(season, team) %>%
  summarise(
    mean_trvl_dst_mtrs = mean(
      if_else(status == 'away_team', travel_dist_meters, NA_real_), na.rm = TRUE
    ),
    
    mean_alt_chg = mean(
      if_else(status == 'away_team', alt_chg, NA_real_), na.rm = TRUE
    ),
    alt_inc = sum(
      status == 'away_team' & alt_chg_desc == 'increase', na.rm = TRUE
    ),
    alt_dec = sum(
      status == 'away_team' & alt_chg_desc == 'decrease', na.rm = TRUE
    ),
    
    mean_tz_chg = mean(
      if_else(status == 'away_team', tz_chg, NA_real_), na.rm = TRUE
    ),
    tz_inc = sum(
      status == 'away_team' & tz_chg_desc == 'forward', na.rm = TRUE
    ),
    tz_dec = sum(
      status == 'away_team' & tz_chg_desc == 'backward', na.rm = TRUE
    ),
    
    mean_home_rest = mean(
      if_else(status == 'home_team', home_rest, NA_integer_), na.rm = TRUE
    ),
    mean_away_rest = mean(
      if_else(status == 'away_team', away_rest, NA_integer_), na.rm = TRUE
    ),
    .groups = 'drop'
  )
  
# -------------------------------------------------------------------------
# Team Streaks Dataset ----------------------------------------------------

streaks_pbp = temp_schedules_pbp %>%
  dplyr::select(
    game_id, season, week, away_team, home_team, div_game, tz_chg
  ) %>%
  pivot_longer(
    cols = c(away_team, home_team),
    names_to = 'home_away',
    values_to = 'team'
  ) %>%
  mutate(
    tz_chg_flag = if_else(home_away == 'away_team' & tz_chg != 0, 1, 0)
  ) %>%
  arrange(team, season, week)

streaks_pbp = streaks_pbp %>%
  group_by(season, team) %>%
  mutate(
    b2b_div_games = if_else(
      div_game == 1 & lag(div_game, default = 0) == 1, 1, 0
    ),
    seq_div_games = sequence(rle(div_game)$lengths) * div_game,
    b2b_tz_chgs = if_else(
      tz_chg_flag == 1 & lag(tz_chg_flag, default = 0) == 1, 1, 0
    ),
    seq_tz_chgs = sequence(rle(tz_chg_flag)$lengths) * tz_chg_flag
  ) %>%
  ungroup()

team_streaks = streaks_pbp %>%
  group_by(season, team) %>%
  summarise(
    b2b_div_games = sum(b2b_div_games, na.rm = TRUE),
    seq_div_games = max(seq_div_games, na.rm = TRUE),
    b2b_tz_chgs = sum(b2b_tz_chgs, na.rm = TRUE),
    seq_tz_chgs = max(seq_tz_chgs, na.rm = TRUE),
    .groups = 'drop'
  )

# -------------------------------------------------------------------------
# Team PBP Dataset --------------------------------------------------------

team_pbp = stats_pbp %>%
  left_join(
    y = demos_pbp,
    by = join_by(season, game_id, week, play_id, team, kicker_player_id)
  ) %>%
  left_join(
    y = env_pbp,
    by = join_by(season, game_id, week, play_id, team)
  ) %>%
  left_join(
    y = records_pbp %>%
      dplyr::select(-seq_le_500, -seq_gt_500),
    by = join_by(season, game_id, team)
  ) %>%
  left_join(
    y = schedules_pbp %>%
      dplyr::select(-status),
    by = join_by(season, game_id, week, team)
  ) %>%
  left_join(
    y = streaks_pbp %>%
      dplyr::select(
        season, game_id, week, team, div_game, b2b_div_games, b2b_tz_chgs
      ),
    by = join_by(season, game_id, week, team)
  )

team_pbp = team_pbp %>%
  mutate(team = as.factor(team))

# -------------------------------------------------------------------------
# Saving Team PBP Data ----------------------------------------------------

team_pbp_file = here(
  'kickers', 'data', 'analytic', 'team_pbp', 'team_pbp.rdata'
)

if (file.exists(team_pbp_file)) {
  file.remove(team_pbp_file)
  cat(basename(team_pbp_file), 'has been deleted.\n')
} else {
  cat(basename(team_pbp_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(team_pbp, file = team_pbp_file)
if (file.exists(team_pbp_file)) {
  cat(basename(team_pbp_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(team_pbp_file), '\n')
}

# -------------------------------------------------------------------------
# Team Summary Dataset ----------------------------------------------------

team_smmry = team_stats %>%
  left_join(
    y = team_demos,
    by = join_by(season == season, team == team)
  ) %>%
  left_join(
    y = team_env,
    by = join_by(season == season, team == team)
  ) %>%
  left_join(
    y = team_records,
    by = join_by(season == season, team == team)
  ) %>%
  left_join(
    y = team_schedules,
    by = join_by(season == season, team == team)
  ) %>%
  left_join(
    y = team_streaks,
    by = join_by(season == season, team == team)
  )

team_smmry = team_smmry %>%
  mutate(team = as.factor(team))

# -------------------------------------------------------------------------
# Saving Team Summary Data ------------------------------------------------

team_smmry_file = here(
  'kickers', 'data', 'analytic', 'team_smmry', 'team_smmry.rdata'
)

if (file.exists(team_smmry_file)) {
  file.remove(team_smmry_file)
  cat(basename(team_smmry_file), 'has been deleted.\n')
} else {
  cat(basename(team_smmry_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(team_smmry, file = team_smmry_file)
if (file.exists(team_smmry_file)) {
  cat(basename(team_smmry_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(team_smmry_file), '\n')
}

# -------------------------------------------------------------------------
# Saving Stadium Data -----------------------------------------------------

stadiums_file = here('kickers', 'data', 'stadiums', 'stadiums.rdata')

if (file.exists(stadiums_file)) {
  file.remove(stadiums_file)
  cat(basename(stadiums_file), 'has been deleted.\n')
} else {
  cat(basename(stadiums_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(stadiums, file = stadiums_file)
if (file.exists(stadiums_file)) {
  cat(basename(stadiums_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(stadiums_file), '\n')
}

# -------------------------------------------------------------------------
# Saving Team Stadiums Data -----------------------------------------------

team_stadiums_file = here('kickers', 'data', 'stadiums', 'team_stadiums.rdata')

if (file.exists(team_stadiums_file)) {
  file.remove(team_stadiums_file)
  cat(basename(team_stadiums_file), 'has been deleted.\n')
} else {
  cat(basename(team_stadiums_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(team_stadiums, file = team_stadiums_file)
if (file.exists(team_stadiums_file)) {
  cat(basename(team_stadiums_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(team_stadiums_file), '\n')
}

# -------------------------------------------------------------------------