# Loading Dependencies ----------------------------------------------------

rm(list = ls())

packages = c(
  'tidyverse',
  'here',
  'plotly',
  'corrplot',
  'Hmisc',
  'broom',
  'gt',
  'purrr',
  'nflfastR'
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
load(kicker_trends_file)

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
    season,
    starters, total_kickers, starter_coverage, 
    starter_fga, total_fga, fga_coverage
  ) %>%
  arrange(season)

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
# Kick Distance and Accuracy: Indoor vs Outdoor Stadiums ------------------

in_vs_out = pbp_pp %>%
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

x_col = 'season'
y_cols = c('fg_pct', 'fg_att', 'med_kck_dst')
group_col = 'roof_clean'
grouped_line_plot(in_vs_out, x_col, y_cols, group_col)

# -------------------------------------------------------------------------
# Total FGA by Season -----------------------------------------------------

#' These tests show there is a statistically significant increase in the number
#' of FG attempts indoors
indoor_fga = in_vs_out %>%
  filter(roof_clean == 'indoors') %>%
  pull(fg_att)

outdoor_fga = in_vs_out %>%
  filter(roof_clean == 'outdoors') %>%
  pull(fg_att)

total_fga = indoor_fga + outdoor_fga
years = 2001:2025
prop.trend.test(x = indoor_fga, n = total_fga, score = years)

# -------------------------------------------------------------------------
# Roof Type by Season -----------------------------------------------------

stadium_roofs = pbp_pp %>%
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
# Indoor Stadium Growth by Season -----------------------------------------

#' These tests show that the number of potentially indoor stadiums has not
#' increased over time
dome = stadium_roofs %>%
  filter(roof_type == 'Dome') %>%
  pull(count)

retractable = stadium_roofs %>%
  filter(roof_type == 'Retractable') %>%
  pull(count)

#' There were no retractable stadium roofs in 2001
#' Adding a zero equalizes the vectors in the addition step below
retractable = c(0, retractable)

indoor = dome + retractable

outdoor = stadium_roofs %>%
  filter(roof_type == 'Outdoors') %>%
  pull(count)

total_stdms = dome + retractable + outdoor
years = 2001:2025
prop.trend.test(x = indoor, n = total_stdms, score = years)

prop.trend.test(x = retractable, n = total_stdms, score = years)

# -------------------------------------------------------------------------
# FG Result by Roof Type --------------------------------------------------

chisq_data = pbp_pp %>%
  dplyr::select(fg_result_clean, roof_clean) %>%
  mutate(
    fg_result_clean = as.factor(fg_result_clean),
    roof_clean = as.factor(roof_clean)
  )
chisq_tbl = table(chisq_data$roof_clean, chisq_data$fg_result_clean)
chisq.test(chisq_tbl)

# -------------------------------------------------------------------------
# Kick Distance by Roof Type ----------------------------------------------

wilcox_data = pbp_pp %>%
  dplyr::select(kick_distance, roof_clean) %>%
  mutate(roof_clean = as.factor(roof_clean))

wilcox.test(
  formula = kick_distance ~ roof_clean, data = wilcox_data, 
  alternative = 'two.sided', conf.int = TRUE
)

# -------------------------------------------------------------------------
# FG% by Roof Type over Season --------------------------------------------

lm_data = pbp_pp %>%
  dplyr::select(season, roof_clean, kick_distance, fg_result_clean) %>%
  mutate(
    roof = as.factor(roof_clean),
    roof = relevel(roof, ref = 'outdoors')
  ) %>%
  group_by(season, roof) %>%
  summarise(
    med_kck_dst = median(kick_distance, na.rm = TRUE),
    fg_att = n(),
    fg_made = sum(fg_result_clean == 'made'),
    fg_pct = fg_made/fg_att,
    .groups = 'drop'
  )

fgp_mod = lm(fg_pct ~ roof + season + roof*season, data = lm_data)

grid = expand.grid(
  season = seq(
    from = min(lm_data$season), to = max(lm_data$season), length.out = 100
  ),
  roof = unique(lm_data$roof)
)

t = qt(0.975, df = fgp_mod$df.residual)
preds = predict(fgp_mod, newdata = grid, se.fit = TRUE)
grid$y_fit = preds$fit
grid$y_low = preds$fit - (t * preds$se.fit)
grid$y_high = preds$fit + (t * preds$se.fit)

plot_ly() %>%
  add_markers(
    data = lm_data, x = ~season, y = ~fg_pct, color = ~roof,
    colors = c('#E69F00', '#56B4E9'),
    legendgroup = ~roof,
    customdata = ~roof,
    hovertemplate = paste0(
      'Season: %{x}<br>',
      'FG%: %{y:.2%}<br>',
      'Roof Type: %{customdata}',
      '<extra></extra>'
    )
  ) %>%
  add_ribbons(
    data = grid, x = ~season, ymin = ~y_low, ymax = ~y_high, 
    color = ~roof, opacity = 0.2, 
    legendgroup = ~roof, showlegend = FALSE,
    hoverinfo = 'none'
  ) %>%
  add_lines(
    data = grid, x = ~season, y = ~y_fit, color = ~roof,
    line = list(width = 3), 
    legendgroup = ~roof, showlegend = FALSE,
    hovertemplate = '<b>Trend:</b> %{y:.2%}<extra></extra>'
  ) %>%
  layout(
    title = 'FG% Convergence: Native Plotly Approach',
    yaxis = list(title = 'FG%', tickformat = '.0%'),
    xaxis = list(title = 'Season'),
    hovermode = 'none'
  )

# -------------------------------------------------------------------------
# Median Kick Distance by Roof Type over Season ---------------------------

med_kd_null_mod = lm(med_kck_dst ~ season, data = lm_data)
med_kd_prtl_mod = lm(med_kck_dst ~ roof + season, data = lm_data)
med_kd_full_mod = lm(med_kck_dst ~ roof + season + roof*season, data = lm_data)

ftest_prtl = anova(med_kd_null_mod, med_kd_prtl_mod, test = 'F') %>%
  tidy() %>%
  filter(!is.na(p.value)) %>%
  pull(p.value)

ftest_full = anova(med_kd_prtl_mod, med_kd_full_mod, test = 'F') %>%
  tidy() %>%
  filter(!is.na(p.value)) %>%
  pull(p.value)

# -------------------------------------------------------------------------
# Visualizing Good and Bad Weather Games ----------------------------------

weather_data = pbp_pp %>%
  dplyr::select(season, game_id, weather_clean) %>%
  distinct() %>%
  mutate(weather = if_else(weather_clean == 'bad', weather_clean, 'good')) %>%
  group_by(season, weather) %>%
  summarise(count = n(), .groups = 'drop')

xcol = 'season'
ycol = c('count')
group_col = 'weather'
grouped_line_plot(weather_data, xcol, ycol, group_col)

# -------------------------------------------------------------------------
# Visualizing FG Stats by Weather -----------------------------------------

weather_impact = pbp_pp %>%
  dplyr::select(
    season, roof_clean, weather_clean, kick_distance, fg_result_clean
  ) %>%
  filter(roof_clean == 'outdoors') %>%
  mutate(
    weather = if_else(weather_clean == 'bad', weather_clean, 'good'),
    weather = factor(weather, levels = c('bad', 'good'))
  ) %>%
  group_by(season, weather) %>%
  summarise(
    med_kck_dst = median(kick_distance, na.rm = TRUE),
    fg_att = n(),
    fg_made = sum(fg_result_clean == 'made'),
    fg_pct = fg_made/fg_att,
    .groups = 'drop'
  )

x_col = 'season'
y_cols = c('fg_pct', 'fg_att', 'med_kck_dst')
group_col = 'weather'
grouped_line_plot(weather_impact, x_col, y_cols, group_col)

# -------------------------------------------------------------------------
# FG% by Weather ----------------------------------------------------------

outdoor_kicks = pbp_pp %>%
  dplyr::select(
    season, weather_clean, roof_clean, kick_distance, fg_result_clean
  ) %>%
  dplyr::filter(roof_clean == 'outdoors') %>%
  mutate(
    weather = if_else(weather_clean == 'bad', weather_clean, 'good'),
    weather = factor(weather, levels = c('bad', 'good')),
    season = season - min(season)
  ) %>%
  group_by(season, weather) %>%
  summarise(
    med_kck_dst = median(kick_distance, na.rm = TRUE),
    fg_att = n(),
    fg_made = sum(fg_result_clean == 'made'),
    fg_pct = fg_made/fg_att,
    .groups = 'drop'
  )

fgp_null_mod = lm(fg_pct ~ season, data = outdoor_kicks)
fgp_prtl_mod = lm(fg_pct ~ weather + season, data = outdoor_kicks)
fgp_full_mod = lm(fg_pct ~ weather + season + weather*season, data = outdoor_kicks)

ftest_prtl = anova(fgp_null_mod, fgp_prtl_mod, test = 'F') %>%
  tidy() %>%
  filter(!is.na(p.value)) %>%
  pull(p.value)

ftest_full = anova(fgp_prtl_mod, fgp_full_mod, test = 'F') %>%
  tidy() %>%
  filter(!is.na(p.value)) %>%
  pull(p.value)

# -------------------------------------------------------------------------
# Median Kick Distance by Weather -----------------------------------------

mkd_null_mod = lm(med_kck_dst ~ season, data = outdoor_kicks)
mkd_prtl_mod = lm(med_kck_dst ~ weather + season, data = outdoor_kicks)
mkd_full_mod = lm(
  med_kck_dst ~ weather + season + weather*season, data = outdoor_kicks
)

ftest_prtl = anova(mkd_null_mod, mkd_prtl_mod, test = 'F') %>%
  tidy() %>%
  filter(!is.na(p.value)) %>%
  pull(p.value)

ftest_full = anova(mkd_prtl_mod, mkd_full_mod, test = 'F') %>%
  tidy() %>%
  filter(!is.na(p.value)) %>%
  pull(p.value)

# -------------------------------------------------------------------------
# xFG Model ---------------------------------------------------------------

xfg_data = pbp_pp %>% 
  dplyr::select(
    kicker_player_id, kicker_name_clean, 
    fg_result_clean, 
    kick_distance, roof_clean, weather_clean, season
  ) %>%
  mutate(
    season = season - min(season),
    fgr = if_else(fg_result_clean == 'made', 1, 0),
    roof = if_else(roof_clean == 'outdoors', 1, 0),
    weather = if_else(weather_clean == 'bad', 1, 0),
  )
xfg_mod = glm(
  formula = fgr ~ kick_distance + season + roof + roof:weather, 
  data = xfg_data,
  family = 'binomial'
)

# -------------------------------------------------------------------------
# FG Success Probability Curves -------------------------------------------

min_kd = min(xfg_data$kick_distance)
max_kd = max(xfg_data$kick_distance)

plot_data = expand.grid(
  kick_distance = min_kd:max_kd,
  roof = c(0, 1),
  weather = c(0, 1),
  season = 24
) %>%
  filter(!(roof == 0 & weather == 1)) %>%
  mutate(
    prob = predict(xfg_mod, newdata = ., type = 'response'),
    scenario = case_when(
      roof == 0 ~ 'Indoors',
      roof == 1 & weather == 0 ~ 'Outdoors (Good Weather)',
      roof == 1 & weather == 1 ~ 'Outdoors (Bad Weather)'
    )
  )

plot_ly(
  plot_data, 
  x = ~kick_distance, 
  y = ~prob, 
  color = ~scenario, 
  colors = c('#2ecc71', '#e74c3c', '#3498db'),
  type = 'scatter', 
  mode = 'lines',
  hovertemplate = paste(
    '%{fullData.name}<br>',
    'Distance: %{x} yards<br>',
    'Prob: %{y:.1%}<extra></extra>'
  )
) %>%
  layout(
    title = 'FG Probability by Distance',
    xaxis = list(title = 'Kick Distance (Yards)'),
    yaxis = list(title = 'Success Probability', tickformat = '.0%'),
    hovermode = 'x unified',
    legend = list(orientation = 'h', y = -0.2)
  )

# -------------------------------------------------------------------------
# 2025 xFGOE Leaderboard --------------------------------------------------

strtrs = kicker_trends %>%
  dplyr::select(season, kicker_name_clean, kicker_player_id) %>%
  mutate(season = season - 2001)

xfg_predictions = xfg_data %>%
  mutate(
    exp_fgr = predict(xfg_mod, type = 'response'),
    xfgoe = fgr - exp_fgr
  )

xfgoe_ldrbrd = xfg_predictions %>%
  inner_join(
    y = strtrs,
    by = join_by(season, kicker_player_id, kicker_name_clean)
  ) %>%
  filter(season == 24) %>%
  group_by(kicker_player_id, kicker_name_clean) %>% 
  dplyr::summarize(
    attempts = n(),
    makes = sum(fgr),
    exp_makes = round(sum(exp_fgr), 1),
    total_xfgoe = round(sum(xfgoe), 2),
    xfgoe_per_kck = round(mean(xfgoe), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_xfgoe))

# -------------------------------------------------------------------------
# Visualizing the 2025 xFGOE Leaderboard ----------------------------------

plot_data = xfgoe_ldrbrd %>%
  slice_max(total_xfgoe, n = 20) %>%
  mutate(
    marker_color = if_else(total_xfgoe > 0, '#2ecc71', '#e74c3c'),
    kicker_label = reorder(kicker_name_clean, total_xfgoe),
    hover_text = paste(
      '<b>Kicker:</b>', kicker_name_clean, '<br>',
      '<b>Season: 2025</b><br>',
      '<b>xFGOE:</b>', total_xfgoe, ' kicks added<br>',
      '-----------------------<br>',
      '<b>Attempts:</b>', attempts, '<br>',
      '<b>Success:</b>', makes, 'Actual /', exp_makes, 'Expected'
    )
  )

plot_ly(plot_data) %>%
  add_segments(
    x = 0, xend = ~total_xfgoe, 
    y = ~kicker_label, yend = ~kicker_label,
    line = list(color = '#aaaaaa', width = 1),
    showlegend = FALSE, hoverinfo = 'none'
  ) %>%
  add_markers(
    x = ~total_xfgoe, 
    y = ~kicker_label,
    marker = list(
      size = 12, color = ~marker_color, 
      line = list(color = '#ffffff', width = 1)
    ),
    text = ~hover_text,
    hoverinfo = 'text',
    name = 'Performance'
  ) %>%
  layout(
    title = list(
      text = paste(
        '2025 Top 20 NFL Kickers<br>',
        'Based on Value Added (xFGOE)'
      ),
      y = 0.95
    ),
    xaxis = list(title = 'Total Kicks Added Above League Average'),
    yaxis = list(title = ""),
    margin = list(l = 150, r = 20, b = 60, t = 80),
    paper_bgcolor = '#f8f9fa', plot_bgcolor = '#f8f9fa'
  )

# -------------------------------------------------------------------------
# Creating the Leaderboard Data for the Shiny App -------------------------

team_logos = teams_colors_logos %>%
  dplyr::select(team_abbr, team_logo_espn)

ldrbrd_data = pbp_pp %>%
  filter(season == 2025) %>%
  arrange(game_id, week, kicker_name_clean) %>%
  dplyr::select(
    kicker_name_clean, season_fg_pct,
    roof_clean, weather_clean, posteam
  ) %>%
  group_by(kicker_name_clean) %>%
  summarise(
    team = last(posteam),
    fgp = last(season_fg_pct),
    indoor_kcks = sum(roof_clean == 'indoors'),
    outdoor_kcks = sum(roof_clean != 'indoors'),
    bad_wthr_kcks = sum(weather_clean == 'bad'),
    good_wthr_kcks = sum(weather_clean != 'bad'),
    .groups = 'drop'
  ) %>%
  left_join(
    y = pbp_pp %>% 
      dplyr::select(kicker_name_clean, headshot) %>%
      distinct(), 
    by = join_by(kicker_name_clean)
  )
xfgoe_ldrbrd  = xfgoe_ldrbrd %>%
  left_join(
    y = ldrbrd_data,
    by = join_by(kicker_name_clean)
  ) %>%
  left_join(
    y = team_logos,
    by = join_by(team == team_abbr)
  )

# -------------------------------------------------------------------------
# Saving the 2025 xFGOE Leaderboard ---------------------------------------

xfgoe_ldrbrd_file = here(
  'kickers', 
  'shiny_apps', 
  'xfgoe_ldrbrd_v1.rdata'
)

if (file.exists(xfgoe_ldrbrd_file)) {
  file.remove(xfgoe_ldrbrd_file)
  cat(basename(xfgoe_ldrbrd_file), 'has been deleted.\n')
} else {
  cat(basename(xfgoe_ldrbrd_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(xfgoe_ldrbrd, file = xfgoe_ldrbrd_file)
if (file.exists(xfgoe_ldrbrd_file)) {
  cat(basename(xfgoe_ldrbrd_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(xfgoe_ldrbrd_file), '\n')
}

# -------------------------------------------------------------------------