# Loading Dependencies ----------------------------------------------------

rm(list = ls())

packages = c(
  'tidyverse',
  'here',
  'plotly',
  'corrplot',
  'Hmisc',
  'lsmeans',
  'car',
  'MASS',
  'gtsummary',
  'lme4'
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

pbp_pp_file = here('kickers', 'data', 'pp', 'pbp', 'pbp_pp.rdata')
load(pbp_pp_file)

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
load(league_trends_file)

team_pbp_file = here(
  'kickers',
  'data',
  'analytic',
  'team_pbp',
  'team_pbp.rdata'
)
load(team_pbp_file)

team_smmry_file = here(
  'kickers',
  'data',
  'analytic',
  'team_smmry',
  'team_smmry.rdata'
)
load(team_smmry_file)

stadiums_file = here(
  'kickers',
  'data',
  'stadiums',
  'stadiums.rdata'
)
load(stadiums_file)

team_stadiums_file = here(
  'kickers',
  'data',
  'stadiums',
  'team_stadiums.rdata'
)
load(team_stadiums_file)

# -------------------------------------------------------------------------
# Determining the Starter Coverage per Season -----------------------------

#' Addressing previous blog's use of only starters
league_totals = pbp_pp %>%
  group_by(season) %>%
  summarise(
    total_kickers = n_distinct(kicker_player_id),
    total_fga = n(), 
    .groups = 'drop'
  )

starters = kicker_trends %>%
  group_by(season) %>%
  summarise(
    starters = n_distinct(kicker_player_id),
    starter_fga = sum(total_fga),
    .groups = 'drop'
  )

starter_coverage = league_totals %>%
  inner_join(y = starters, by = join_by(season)) %>%
  mutate(
    starter_coverage = starters / total_kickers,
    fga_coverage = starter_fga / total_fga
  ) %>%
  dplyr::select(
    starters, total_kickers, starter_coverage, 
    starter_fga, total_fga, fga_coverage
  )

# -------------------------------------------------------------------------
# Environmental Correlations ----------------------------------------------

cor_data_env = team_pbp %>%
  mutate(
    bin_fg_result = if_else(fg_result_clean == 'made', 1, 0),
    bin_weather = if_else(weather_clean == 'bad', 1, 0),
    bin_surface = if_else(surface_clean == 'grass', 1, 0),
    bin_roof = if_else(roof_clean == 'outdoors', 1, 0),
    bin_start_time = if_else(start_time_clean == 'night', 1, 0)
  ) %>%
  dplyr::select(
    bin_fg_result, kick_distance,
    wind_clean, temp_clean,
    bin_weather, bin_surface, bin_roof, bin_start_time
  )

cor_mat_env = cor(cor_data_env, use = 'pairwise.complete.obs')
corrplot(
  cor_mat_env, method='color', type='upper', 
  tl.cex=0.5, tl.srt=45, cl.pos='b', mar=c(0, 0, 4, 0)
)

# -------------------------------------------------------------------------
# Roof Type Over Season ---------------------------------------------------

stadium_roofs = team_pbp %>%
  dplyr::select(season, stadium_id) %>%
  left_join(y = stadiums %>% 
              dplyr::select(stadium_id, roof_type),
            by = join_by(stadium_id)) %>%
  distinct() %>%
  group_by(season, roof_type) %>%
  summarise(count = n(), .groups = 'drop')

x_col = 'season'
y_cols = c('count')
group_col = 'roof_type'
grouped_line_plot(stadium_roofs, x_col, y_cols, group_col)

# -------------------------------------------------------------------------
# Kick Distance and Accuracy: Indoor vs Outdoor Stadiums ------------------

in_vs_out = team_pbp %>%
  dplyr::select(
    season, roof_clean, kick_distance, fg_result_clean
  ) %>%
  mutate(roof_clean = as.factor(roof_clean)) %>%
  group_by(season, roof_clean) %>%
  summarise(
    med_kck_dst = median(kick_distance, na.rm = TRUE),
    fg_att = n(),
    fg_made = sum(fg_result_clean == 'made'),
    fg_pct = fg_made/fg_att,
    .groups = 'drop'
  )

y_cols = c('fg_pct', 'fg_att', 'med_kck_dst')
group_col = 'roof_clean'
grouped_line_plot(in_vs_out, x_col, y_cols, group_col)

# -------------------------------------------------------------------------
# Indoor vs Outdoor Stadiums over Season ----------------------------------

#' These tests show that the number of potentially indoor stadiums has not
#' increased over time
domes = stadium_roofs %>%
  filter(roof_type == 'Dome') %>%
  pull(count)

retractables = stadium_roofs %>%
  filter(roof_type == 'Retractable') %>%
  pull(count)

#' There were no retractable stadium roofs in 2001
#' Adding a zero equalizes the vectors in the addition step below
retractables = c(0, retract_stdms)

indoors = dome_stdms + retract_stdms

outdoors = stadium_roofs %>%
  filter(roof_type == 'Outdoors') %>%
  pull(count)

total_stdms = domes + retractables + outdoors
years = 2001:2025

prop.trend.test(x = indoors, n = total_stdms, score = years)

# -------------------------------------------------------------------------
# FGA over Season ---------------------------------------------------------

#' These tests show there is a statistically significant increase in the number
#' of FG attempts and FG conversions indoors
indoor_fga = in_vs_out %>%
  filter(roof_clean == 'indoors') %>%
  pull(fg_att)

outdoor_fga = in_vs_out %>%
  filter(roof_clean == 'outdoors') %>%
  pull(fg_att)

total_fga = indoor_fga + outdoor_fga
years = 2001:2025
prop.trend.test(x = indoor_fga, n = total_fga, score = years)

fgp_roof_mod = lm(fg_pct ~ roof_clean + season, data = in_vs_out)
summary(fgp_roof_mod)
tbl_regression(
  fgp_roof_mod,
  estimate_fun = label_style_sigfig(digits = 3),
  pvalue_fun = label_style_pvalue(digits = 3)
) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()






test_stdm = team_pbp %>%
  dplyr::select(fg_result_clean, kick_distance, roof_clean, season) %>%
  mutate(
    fg_result_clean = as.factor(fg_result_clean),
    roof_clean = as.factor(roof_clean)
  )
stdm_cntgncy_tbl = table(test_stdm$roof_clean, test_stdm$fg_result_clean)
stdm_chisq = chisq.test(stdm_cntgncy_tbl)
stdm_chisq$expected

kck_dst_stdm_mod = lm(kick_distance ~ roof_clean + season, data = test_stdm)
tbl_regression(
  kck_dst_stdm_mod,
  estimate_fun = label_style_sigfig(digits = 3),
  pvalue_fun = label_style_pvalue(digits = 3)
) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# -------------------------------------------------------------------------
# Visuals Start Time ------------------------------------------------------

early_mid_late = team_pbp %>%
  dplyr::select(
    season, start_time_clean, kick_distance, fg_result_clean
  ) %>%
  mutate(start_time_clean = as.factor(start_time_clean)) %>%
  group_by(season, start_time_clean) %>%
  summarise(
    med_kck_dst = median(kick_distance),
    fg_att = n(),
    fg_made = sum(fg_result_clean == 'made'),
    fg_pct = fg_made/fg_att,
    .groups = 'drop'
  )

y_cols = c('fg_pct', 'fg_att', 'med_kck_dst')
group_col = 'start_time_clean'
grouped_line_plot(early_mid_late, x_col, y_cols, group_col)

# -------------------------------------------------------------------------
# Testing Start Time ------------------------------------------------------

fgpct_strt_tm_mod = lm(
  fg_pct ~ start_time_clean + season, 
  data = early_mid_late
)
tbl_regression(
  fgpct_strt_tm_mod,
  estimate_fun = label_style_sigfig(digits = 3),
  pvalue_fun = label_style_pvalue(digits = 3)
) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

test_strt_tm = team_pbp %>%
  dplyr::select(fg_result_clean, kick_distance, start_time_clean, season) %>%
  mutate(
    fg_result_clean = as.factor(fg_result_clean),
    start_time_clean = as.factor(start_time_clean)
  )
strt_tm_cntgncy_tbl = table(
  test_strt_tm$start_time_clean,
  test_strt_tm$fg_result_clean
)
strt_tm_chisq = chisq.test(strt_tm_cntgncy_tbl)
strt_tm_chisq$expected

kck_dst_strt_tm_mod = lm(
  kick_distance ~ start_time_clean + season,
  data = test_strt_tm
)
tbl_regression(
  kck_dst_strt_tm_mod,
  estimate_fun = label_style_sigfig(digits = 3),
  pvalue_fun = label_style_pvalue(digits = 3)
) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

# -------------------------------------------------------------------------
# Teams that Changed Roof Type --------------------------------------------

tms_to_track = team_stadiums %>%
  group_by(stadium) %>%
  filter(year(last_game_date) >= 2001) %>%
  ungroup() %>%
  group_by(team_fastr) %>%
  filter(
    n_distinct(stadium) > 1 & n_distinct(roof_type) > 1,
    !(team_fastr %in% c('MIN', 'NO', 'BUF'))
  ) %>%
  pull(team_fastr) %>%
  unique()

stdm_lookup = team_stadiums %>%
  filter(team_fastr %in% tms_to_track) %>%
  dplyr::select(
    team_fastr, stadium, roof_type, first_game_date, last_game_date, is_current
  ) %>%
  rename(stdm_to_track = stadium, roof_chg = roof_type) %>%
  mutate(yrs_used = year(last_game_date) - year(first_game_date))

#' Merge pbp data to determine number of fga between first and last game date
#' Merge pbp clean data to get head coach info
#' Assess whether fga, fg_pct, and kick distance changed for these teams
# -------------------------------------------------------------------------
# FG Stats Before and After Changing Roof Type ----------------------------

stdm_stats_pbp = pbp_pp %>%
  inner_join(
    y = stdm_lookup, 
    by = join_by(
      home_team == team_fastr,
      game_date >= first_game_date,
      game_date <= last_game_date,
      stadium_id == stdm_to_track
    )
  )

stdm_stats_smmry = stdm_stats_pbp %>%
  group_by(home_team, stadium_id, roof_chg) %>%
  summarise(
    n_kickers = n_distinct(kicker_player_id), 
    med_kck_dst = median(kick_distance, na.rm = TRUE),
    fga = n(),
    fgm = sum(fg_result_clean == 'made', na.rm = TRUE),
    fg_pct = fgm/fga,
    .groups = 'drop'
  ) %>%
  left_join(
    y = stdm_stats_pbp %>%
      dplyr::select(stadium_id, yrs_used, is_current) %>%
      distinct(),
    by = join_by(stadium_id)
  ) %>%
  mutate(
    home_team = as.factor(home_team),
    fga_per_yr = if_else(yrs_used != 0, fga/yrs_used, 0),
    fgm_per_yr = if_else(yrs_used != 0, fgm/yrs_used, 0)
  ) %>%
  arrange(home_team, is_current) %>%
  group_by(home_team) %>%
  mutate(
    chg_yrly_fga = fga_per_yr - lag(fga_per_yr, n = 1, default = 0),
    chg_yrly_fgm = fgm_per_yr - lag(fgm_per_yr, n = 1, default = 0),
    chg_fg_pct = fg_pct - lag(fg_pct, n = 1, default = 0)
  ) %>%
  ungroup()

# -------------------------------------------------------------------------
# Visualizing the Effect of Stadium Change --------------------------------

group_col = 'home_team'
chg_cols = c('chg_yrly_fga', 'chg_yrly_fgm', 'chg_fg_pct')
stadium_impact_plot(stdm_stats_smmry, chg_cols, group_col)

# -------------------------------------------------------------------------
































# Schedule Correlations ---------------------------------------------------

cor_data_all_tms = team_pbp %>%
  mutate(bin_fg_result = if_else(fg_result_clean == 'made', 1, 0)) %>%
  dplyr::select(
    bin_fg_result, kick_distance,
    gt_500, b2b_gt_500, le_500, b2b_le_500,
    away_rest, home_rest,
    div_game, b2b_div_games
  )

cor_mat_all_tms = cor(cor_data_all_tms, use = 'pairwise.complete.obs')
corrplot(
  cor_mat_all_tms, method='color', type='upper', 
  tl.cex=0.5, tl.srt=45, cl.pos='b', mar=c(0, 0, 4, 0)
)

cor_data_awy_tms = team_pbp %>%
  filter(status == 'away_team') %>%
  mutate(bin_fg_result = if_else(fg_result_clean == 'made', 1, 0)) %>%
  dplyr::select(
    bin_fg_result, kick_distance,
    gt_500, b2b_gt_500, le_500, b2b_le_500,
    away_rest, home_rest,
    div_game, b2b_div_games,
    travel_dist_meters, tz_chg, alt_chg
  )

cor_mat_awy_tms = cor(cor_data_awy_tms, use = 'pairwise.complete.obs')
corrplot(
  cor_mat_awy_tms, method='color', type='upper', 
  tl.cex=0.5, tl.srt=45, cl.pos='b', mar=c(0, 0, 4, 0)
)

# -------------------------------------------------------------------------
# Visuals Scheduling ------------------------------------------------------



# -------------------------------------------------------------------------