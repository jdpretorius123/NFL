# Loading Dependencies ----------------------------------------------------

rm(list = ls())

packages = c(
  'car',
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
# Creating the Team Trends Dataset ----------------------------------------

team_demos = pbp_pp %>%
  arrange(season, posteam, week, game_id, play_id) %>%
  dplyr::select(season, posteam, kicker_player_id, age, height, weight) %>%
  distinct() %>%
  group_by(season, team = posteam) %>%
  summarise(
    n_kickers = n_distinct(kicker_player_id),
    mean_age = mean(age, na.rm = TRUE),
    mean_weight = mean(weight, na.rm = TRUE),
    mean_height = mean(height, na.rm = TRUE),
    .groups = 'drop'
  )

team_stats = pbp_pp %>%
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
  group_by(season, team = posteam) %>%
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
    
    .groups = 'drop'
  )

conditions_vars = c(
  'season',
  'posteam',
  'week',
  'wind_clean',
  'temp_clean',
  'weather_clean',
  'surface_clean',
  'roof_clean',
  'start_time_clean'
)
team_conditions = pbp_pp %>%
  arrange(season, posteam, week, game_id, play_id) %>%
  dplyr::select(all_of(conditions_vars)) %>%
  distinct() %>%
  group_by(season, team = posteam) %>%
  summarise(
    med_wind = median(wind_clean, na.rm = TRUE),
    med_temp = median(temp_clean, na.rm = TRUE),
    
    bad_wthr_gms = sum(weather_clean == 'bad', na.rm = TRUE),
    decent_wthr_gms = sum(weather_clean == 'decent', na.rm = TRUE),
    good_wthr_gms = sum(weather_clean == 'good', na.rm = TRUE),
    prop_bad_wthr_gms = bad_wthr_gms / (decent_wthr_gms + good_wthr_gms),
    
    grass_flds = sum(surface_clean == 'grass', na.rm = TRUE),
    turf_flds = sum(surface_clean == 'turf', na.rm = TRUE),
    prop_grass_flds = grass_flds / (grass_flds + turf_flds),
    
    indoor_gms = sum(roof_clean == 'indoors', na.rm = TRUE),
    outdoor_gms = sum(roof_clean == 'outdoors', na.rm = TRUE),
    prop_outdoor_gms = outdoor_gms / (indoor_gms + outdoor_gms),
    
    morning_gms = sum(start_time_clean == 'morning', na.rm = TRUE),
    afternoon_gms = sum(start_time_clean == 'afternoon', na.rm = TRUE),
    night_gms = sum(start_time_clean == 'night', na.rm = TRUE),
    prop_prime_time = night_gms / (morning_gms + afternoon_gms + night_gms),
    
    .groups = 'drop'
  ) %>%
  dplyr::select(
    season,
    team,
    med_wind, 
    med_temp,
    prop_bad_wthr_gms, 
    prop_grass_flds,
    prop_outdoor_gms,
    prop_prime_time
  )

team_trends = team_stats %>%
  inner_join(y = team_demos, by = join_by(season, team)) %>%
  inner_join(y = team_conditions, by = join_by(season, team))

# -------------------------------------------------------------------------
# Visualizations ----------------------------------------------------------

y_cols = colnames(team_trends)[
  colnames(team_trends) != 'season' & colnames(team_trends) != 'team'
]
dynamic_violin(team_trends, c('season'), y_cols)

team_info = load_teams() %>%
  dplyr::select(team_abbr, team_color, team_color2)
nfl_colors = team_info$team_color
names(nfl_colors) = team_info$team_abbr

nfl_teams_line_plot(team_trends, y_cols)
nfl_teams_heatmap(team_trends, y_cols)
# -------------------------------------------------------------------------
# ANOVA Testing -----------------------------------------------------------

test_vars = y_cols
anova_results = test_vars %>%
  map_df(function(var) {
    formula = as.formula(paste(var, '~ season*team'))
    model = lm(
      formula, 
      data = team_trends, 
      contrasts = list(team = contr.sum)
    )
    
    car::Anova(model, type = 'III') %>%
      tidy() %>%
      mutate(variable = var)
  })

n = length(test_vars)
anova_results = anova_results %>%
  filter(term %in% c('team', 'team:season')) %>%
  group_by(term) %>%
  mutate(
    p_bnfrrni = p.adjust(p.value, method = 'bonferroni', n = n),
    sig = p_bnfrrni < 0.05
  ) %>%
  arrange(term, p_bnfrrni)

# -------------------------------------------------------------------------


