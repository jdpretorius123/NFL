# Installing/Loading Dependencies -----------------------------------------
packages = c('tidyverse',
             'lubridate',
             'plotly',
             'htmlwidgets',
             'corrplot',
             'data.table',
             'ggmosaic')

check_packages = function(packages, repos = 'https://cloud.r-project.org') {
  lapply(packages, function(p) {
    #' Checking if the package is installed
    if (!requireNamespace(p, quietly = TRUE)) {
      message(paste('Package', p, 'not found. Attempting to install...'))
      
      #' Installing the missing package
      install.packages(p, repos = repos)
    }
    
    #' Loading the package
    library(p, character.only = TRUE)
    message(paste('Package', p, 'loaded successfully.'))
  })
}
check_packages(packages)
# -------------------------------------------------------------------------
# Clearing Memory ---------------------------------------------------------
rm(list = ls())
# -------------------------------------------------------------------------
# Loading the Helper Functions --------------------------------------------
source('./helper_functions/helper_functions.R')
# -------------------------------------------------------------------------
# Loading Master Dataset --------------------------------------------------
fname = './data/master/pbp_clean.Rdata'
load(fname)
# -------------------------------------------------------------------------
# Creating a Temporary Dataset --------------------------------------------
temp = pbp_clean %>%
  
  #' Keeping only regular season games
  filter(season_type == 'REG') %>%
  
  #' Creating field goal result indicator
  mutate(fg_result = if_else(field_goal_result != 'made',
                             'missed',
                             'made'),
         
         #' Creating a lag term for fg_result
         lag_fg_result = lag(fg_result, n = 1),
         
         #' Changing drive_time of possession to cumulative seconds elapsed
         drive_top = (minute(drive_time_of_possession)*60) +
           second(drive_time_of_possession),
         
         #' Changing start_time to cumulative seconds elapsed since midnight
         start_time = hour(start_time)*3600 + (minute(start_time)*60) + 
           second(start_time))

#' Double-checking:
#' 1. Only regular season games are kept
#' 2. fg_result, lag_fg_result, drive_top and start_time are created as
#' expected
glimpse(temp)
categorical_cols = c('season_type', 'fg_result', 'lag_fg_result')

categorical_df = temp %>%
  
  #' Selecting the categorical columns listed above
  dplyr::select(all_of(categorical_cols))

#' Creating a frequency table for each categorical column
categorical_list = lapply(categorical_df, table, useNA = 'always')

#' Creating a character vector of variables important for visual analysis
vars = c('play_id', 'game_id', 'season',
         
         'home_team', 'away_team', 'week', 'game_date', 
         
         'start_time', 'roof', 'surface', 'temp', 'wind', 'div_game', 'weather',
         
         'posteam', 'posteam_type', 'defteam',
         
         'game_seconds_remaining', 'drive_top', 'current_drive_play_count',
         
         'defteam_timeouts_remaining', 'posteam_timeouts_remaining',
         
         'play_type', 'special_teams_play', 'score_differential', 'kick_distance',
         
         'kicker_player_name', 'kicker_player_id', 'age', 'height', 'weight',
         
         'field_goal_attempt', 'fg_result', 'lag_fg_result')

#' Selecting the variables listed above
temp = temp %>%
  dplyr::select(all_of(vars))

#' Double-checking the selection step
colnames(temp)
colSums(is.na(temp))
# -------------------------------------------------------------------------
# Calculating each Kicker's Career FG% ------------------------------------
fg_data = temp %>%
  
  #' Keeping only field goal attempts
  filter(field_goal_attempt == 1) %>%
  
  #' Ascend sort by game_id within week within season
  arrange(season, week, game_id) %>%
  
  #' Grouping plays by kicker ID
  group_by(kicker_player_id) %>%
  
  #' Calculating each kicker's career FG% as a rolling percent that changes with
  #' each recorded attempt
  mutate(career_fg_pct = cumsum(fg_result == 'made')/
           cumsum(field_goal_attempt)) %>%
  
  #' Ungrouping the data to facilitate dataset manipulation
  ungroup()
# -------------------------------------------------------------------------
# Calculating each Kicker's Career FG Attempts ----------------------------
fg_data = fg_data %>%
  
  #' Ascend sort by game_id within week within season
  arrange(season, week, game_id) %>%
  
  #' Grouping plays by kicker ID
  group_by(kicker_player_id) %>%
  
  #' Calculating career FG attempts as a rolling total
  mutate(career_fg_attempt = row_number()) %>%
  
  #' Ungrouping the data to facilitate dataset manipulation
  ungroup()
# -------------------------------------------------------------------------
# Calculating Back-to-Back & Sequential FG Makes & Misses -----------------
fg_data = fg_data %>%
  
  #' Ascend sort by game_id within week within season
  arrange(season, week, game_id) %>%
  
  #' Grouping plays by Kicker ID
  group_by(kicker_player_id) %>%
  
  #' Calculating back-to-back (B2B) FG makes and misses
  mutate(
    #' B2B FG Makes
    b2b_fg_makes = if_else(
      (fg_result == 'made') & (lag_fg_result == 'made'), 1, 0
    ),
    b2b_fg_makes = if_else(is.na(b2b_fg_makes), 0, b2b_fg_makes),
    
    #' B2B FG Misses
    b2b_fg_misses = if_else(
      (fg_result == 'missed') & (lag_fg_result == 'missed'), 1, 0
    ),
    b2b_fg_misses = if_else(is.na(b2b_fg_misses), 0, b2b_fg_misses)
  ) %>%
  
  #' Ungrouping the data to facilitate data manipulation
  ungroup() %>%
  
  #' Grouping plays by kicker ID
  group_by(kicker_player_id) %>%
  
  #' Creating a streak ID for fg_result
  #' rleid generates a new streak ID whenever the value of fg_result changes
  mutate(streak_id = data.table::rleid(fg_result)) %>%
  
  #' Ungrouping the data to facilitate manipulation
  ungroup() %>%
  
  #' Calculating sequential FG makes and misses
  #' Grouping by streak ID within kicker ID
  group_by(kicker_player_id, streak_id) %>%
  
  #' Calculating the length of each kicker's FG make or miss streak
  mutate(streak_length = row_number(),
         
         #' Creating streak lengths specific to FG make and miss streaks
         seq_fg_makes = if_else(fg_result == 'made', streak_length, 0),
         seq_fg_misses = if_else(fg_result == 'missed', streak_length, 0)) %>%
  
  #' Ungrouping the data to facilitate manipulation
  ungroup()
# -------------------------------------------------------------------------
# Calculating each Kicker's FG% per Season --------------------------------
fg_data = fg_data %>%
  
  #' Ascend sort by game_id within week within season
  arrange(season, week, game_id) %>%
  
  #' Grouping plays by season within kicker ID
  group_by(kicker_player_id, season) %>%
  
  #' Calculating each kicker's FG% per season as a rolling percent that changes
  #' with each recorded attempt
  mutate(season_fg_pct = cumsum(fg_result == 'made')/
           cumsum(field_goal_attempt)) %>%
  
  #' Ungrouping the data to facilitate manipulation
  ungroup()
# -------------------------------------------------------------------------
# Calculating each Kicker's FG Attempts per Season ------------------------
fg_data = fg_data %>%
  
  #' Ascend sort by game_id within week within season
  arrange(season, week, game_id) %>%
  
  #' Grouping plays by season within kicker ID
  group_by(kicker_player_id, season) %>%
  
  #' Calculating each kicker's FG attempts per season
  mutate(season_fg_attempt = row_number()) %>%
  
  #' Ungrouping the data to facilitate manipulation
  ungroup()
# -------------------------------------------------------------------------
# Double-Checking the Validity of Engineered FG Variables  ----------------
glimpse(fg_data)
colSums(is.na(fg_data))
# -------------------------------------------------------------------------
# Distributions of Career End Points --------------------------------------
#' Creating a Temporary Dataset for Career FG Endpoints
career_endpoints = fg_data %>%
  group_by(kicker_player_id) %>%
  summarise(career_fg_pct = last(career_fg_pct),
            career_fg_attempts = last(career_fg_attempt),
            .groups = 'drop')

#' Histograms of Career Endpoint Data
career_cols = c('career_fg_pct', 'career_fg_attempts')
histograms(career_endpoints, 'career_fg_pct', 50, career_cols)

#' Distributional statistics of Career Endpoint Data
career_endpoint_stats = lapply(career_endpoints %>%
                                 dplyr::select(-kicker_player_id),
                               summary,
                               useNA = 'always')

#' Conclusions:
#' 1. Career FG Attempts Distribution
#'    - Heavily right-skew
#'    - Heavy in the first, smallest bin (0-19.5 career FG attempts)
#'    - 25th percentile = 14 FG attempts
#'    - Median = 89.5 FG attempts
#' 2. Career FG% Distribution
#'    - Mostly normal
#'    - Most kickers in the dataset have a career FG% between 82.5-87.5%
#'    - 25th percentile = 75%
#'    - Median = 82%
# -------------------------------------------------------------------------
# Distributions of Season End Points  -------------------------------------
#' Creating a Temporary Dataset for FG Endpoints per Season
season_endpoints = fg_data %>%
  group_by(kicker_player_id, season) %>%
  summarise(season_fg_pct = last(season_fg_pct),
            season_fg_attempts = last(season_fg_attempt),
            .groups = 'drop')

#' Defining the season end points of interest
y = 'season_fg_pct'
y_cols = c('season_fg_pct', 'season_fg_attempts')

#' Defining the time variable, and its unique values and text for visualization
x = 'season'
x_tickvals = sort(unique(season_endpoints$season))
x_ticktext = as.character(sort(unique(season_endpoints$season)))

#' Visualizing the distributions of end points by season
strip_charts(season_endpoints,
             x, y,
             y_cols,
             x_tickvals, x_ticktext)

#' Defining the list of summary functions to apply to the endpoints by season
summary_funcs = list(
  min    = ~min(., na.rm = TRUE),
  max    = ~max(., na.rm = TRUE),
  mean   = ~mean(., na.rm = TRUE),
  median = ~median(., na.rm = TRUE),
  q25    = ~quantile(., 0.25, na.rm = TRUE),
  q75    = ~quantile(., 0.75, na.rm = TRUE)
)

#' Calculating summary statistics for the end points by season
season_endpoint_stats = grouped_sumstats(season_endpoints,
                                         y_cols,
                                         x,
                                         summary_funcs)
# -------------------------------------------------------------------------
# Confounder and Numeric Variable Distributions ---------------------------
vars = fg_data %>%
  dplyr::select(kicker_player_id,
                wind,
                temp,
                score_differential,
                current_drive_play_count,
                drive_top,
                age,
                height,
                weight,
                drive_top,
                start_time,
                game_seconds_remaining,
                kick_distance) %>%
  distinct()
x = 'age'
nbins = 30
xcols = c('wind',
          'temp',
          'score_differential',
          'current_drive_play_count',
          'drive_top',
          'age',
          'height',
          'weight',
          'drive_top',
          'start_time',
          'game_seconds_remaining',
          'kick_distance')
histograms(vars, x, nbins, xcols)
# -------------------------------------------------------------------------
# Assessing Continuous Variable Interactions with Field Goal Result -------
x = 'fg_result'
y = 'wind'
y_cols = c('wind',
           'temp',
           'score_differential',
           'current_drive_play_count',
           'drive_top',
           'start_time',
           'game_seconds_remaining',
           'kick_distance')
x_tickvals = unique(as.numeric(factor(fg_data$fg_result)))
x_ticktext = levels(factor(fg_data$fg_result))
strip_charts(fg_data, x, y, y_cols, x_tickvals, x_ticktext)
# -------------------------------------------------------------------------
# Assessing Categorical Variable Interactions with Field Goal Result ------
bar_chart(fg_data, 'roof', 'open', 'closed')
bar_chart(fg_data, 'surface', 'turf', 'grass')
bar_chart(fg_data, 'weather', 'precipitation', 'clear')
bar_chart(fg_data, 'div_game', 'divisional', 'non_divisional')
# -------------------------------------------------------------------------
# Assessing Career FG% by Career FG Attempt Count -------------------------
career_fg_pct_x_att = fg_data %>%
  filter(career_fg_attempt %% 10 == 0)
all_kickers_career_plot = plot_ly(data = career_fg_pct_x_att,
                                  x = ~career_fg_attempt,
                                  y = ~career_fg_pct,
                                  color = ~kicker_player_name,
                                  colors = 'Paired',
                                  type = 'scatter',
                                  mode = 'lines',
                                  hoverinfo = 'text',
                                  text = ~paste0(
                                    'Kicker: ', kicker_player_name,
                                    '<br>Attempt: ', career_fg_attempt,
                                    '<br>FG%: ',
                                    sprintf('%.2f', career_fg_pct*100), '%'
                                  ),
                                  line = list(width = 0.5)) %>%
  layout(title = 'Career FG% Over Time for All Kickers',
         xaxis = list(title = 'Career FG Attempt Count'),
         yaxis = list(title = 'Career FG%',
                      tickformat = '.1%'),
         showlegend = FALSE)
all_kickers_career_plot
# -------------------------------------------------------------------------
# Assessing Season FG% by Season for All Kickers --------------------------
all_kickers_season_data = fg_data %>%
  group_by(kicker_player_name, season) %>%
  arrange(game_date) %>%
  summarise(season_fg_attempts = last(season_fg_attempt),
            season_fg_pct = last(season_fg_pct),
            .groups = 'drop')
x = 'season'
y = 'season_fg_pct'
y_cols = c('season_fg_pct', 'season_fg_attempts')
x_tickvals = unique( as.numeric( factor(all_kickers_season_data$season) ) )
x_ticktext = sort( levels( factor(all_kickers_season_data$season) ) )
strip_charts(all_kickers_season_data, x, y, y_cols, x_tickvals, x_ticktext)
# -------------------------------------------------------------------------
# Assessing Field Goal Result by its Lag Term -----------------------------
fg_result_x_lag = fg_data %>%
  dplyr::select(fg_result, lag_fg_result) %>%
  na.omit() %>%
  count(lag_fg_result, fg_result, name = 'count') %>%
  mutate(total = sum(count))

fg_result_x_lag_mosaic = fg_result_x_lag %>%
  ggplot() +
  geom_mosaic(mapping = aes(x = product(lag_fg_result),
                            fill = fg_result,
                            weight = count)) +
  labs(title = 'Current FG Result vs. Previous FG Result',
       x = 'Previous FG Result',
       y = 'Current FG Result',
       fill = 'Current FG Result') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = 'right')

fg_result_x_lag_mosaic = ggplotly(fg_result_x_lag_mosaic)
fg_result_x_lag_mosaic
# -------------------------------------------------------------------------
# Correlation Plots -------------------------------------------------------
#' Defining career attempt milestones
career_att_milestones = seq(from = 0, to = 200, by = 10)

#' Creating the data for FG% along career attempt milestones 
fg_pct_x_attempt = fg_data %>%
  
  #' Keeping only the rows of data that match the FG attempt milestones above
  filter(career_fg_attempt %in% career_att_milestones) %>%
  dplyr::select(kicker_player_name, career_fg_attempt, career_fg_pct) %>%
  
  #' Turning the career attempt milestones into new new column names
  pivot_wider(names_from = career_fg_attempt,
              
              #' Turning the career FG% values along those milestones
              #' into the column values
              values_from = career_fg_pct,
              
              #' Defining a prefix for the new column names
              names_prefix = 'fg_pct_att_')

#' Calculating each kicker's final career FG%
career_fg_pct_x_att = fg_data %>%
  group_by(kicker_player_name) %>%
  summarise(career_fg_pct = sum(fg_result == 'made')/
              sum(field_goal_attempt),
            .groups = 'drop') %>%
  
  #' Joining each kicker's final career FG% with their FG% along each career
  #' attempt milestone
  left_join(y = fg_pct_x_attempt, by = 'kicker_player_name')

#' Removing each kicker's name from the data for the correlation plot
fg_pct_cor_data = career_fg_pct_x_att %>%
  dplyr::select(-kicker_player_name)

#' Creating the correlation matrix and plot
fg_pct_cor_mat = cor(fg_pct_cor_data, use = 'pairwise.complete.obs')
corrplot(fg_pct_cor_mat, method='color', type='upper', tl.cex=0.5,
         tl.srt=45, cl.pos='b', mar=c(0, 0, 4, 0))


#' Creating the data for the correlation plot between field goal result and 
#' potential predictors

#' Defining the variables to use in the correlation matrix
vars_to_select  = c('age', 'height', 'weight',
                    
                    'wind', 'temp', 'surface', 'roof', 'weather',
                    
                    'div_game', 'start_time',
                    
                    'drive_top', 'score_differential', 'game_seconds_remaining',
                    'current_drive_play_count',
                    
                    'defteam_timeouts_remaining', 'posteam_timeouts_remaining',
                    
                    'kick_distance', 
                    
                    'career_fg_pct', 'career_fg_attempt',
                    
                    'b2b_fg_makes', 'seq_fg_makes',
                    
                    'b2b_fg_misses', 'seq_fg_misses',
                    
                    'lag_fg_result', 'fg_result')

fg_result_cor_data = fg_data %>%
  dplyr::select(all_of(vars_to_select)) %>%
  
  #' Coercing character variables to numeric
  mutate(fg_result = if_else(fg_result == 'made', 1, 0),
         lag_fg_result = if_else(lag_fg_result == 'made', 1, 0))

#' Creating the correlation matrix and plot
fg_result_cor_mat = cor(fg_result_cor_data, use = 'pairwise.complete.obs')
corrplot(fg_result_cor_mat, method='color', type='upper', tl.cex=0.5,
         tl.srt=45, cl.pos='b', mar=c(0, 0, 4, 0))

#' Conclusions:
#' These variables show the strongest correlation with Field Goal Result
#'  1. kick distance
#'  2. career field goal percent
#'  3. career field goal attempt
#'  4. back-to-back field goal makes and misses
#'  5. sequential field goal makes and misses
# -------------------------------------------------------------------------
# Implementing Split-Half Reliability -------------------------------------

#' I'm determining the number of FG attempts where FG% stabilizes - the point
#' where the number of FG attempts (the noise - random variance) cancels out
#' enough that true skill (the signal) dominates
attempts = seq(from = 10,
               to = max(career_endpoints$career_fg_attempts),
               by = 10)
results = data.frame(n = integer(), attempts = integer(), corr = double())
for (a in attempts) {
  data = fg_data %>%
    group_by(kicker_player_id) %>%
    arrange(kicker_player_id, career_fg_attempt) %>%
    filter(career_fg_attempt <= a) %>%
    filter(max(career_fg_attempt) >= a) %>%
    ungroup()
  
  split_data = data %>%
    group_by(kicker_player_id) %>%
    mutate(is_good = if_else(fg_result == 'made', 1, 0),
           split_group = ifelse(career_fg_attempt %% 2 == 0,
                                'even_pct',
                                'odd_pct')) %>%
    group_by(kicker_player_id, split_group) %>%
    summarise(pct = mean(is_good, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = split_group, values_from = pct)
  
  r = cor(split_data$even_pct, split_data$odd_pct, use = 'complete.obs')
  r = (2*r)/(1+r)
  
  results = rbind(results,
                  data.frame(n = nrow(split_data), attempts = a, corr = r))
  results = results %>%
    filter(n >= 70)
}
results = results %>%
  filter(corr == max(results$corr))
# -------------------------------------------------------------------------
# Implementing Empirical Bayes Padding ------------------------------------
#' Represents the strength of the prior (league average FG% in this context)
r = results$corr/(2 - results$corr)
padding = ((1-r) / r) * mean(career_endpoints$career_fg_attempts)
mean_fg_pct = mean(career_endpoints$career_fg_pct)

#' Determining the true FG% for each kicker
true_fg_pct = fg_data %>%
  group_by(kicker_player_id) %>%
  filter(max(career_fg_attempt) >= results$attempts) %>%
  summarise(true_fg_pct = ( sum(fg_result == 'made') + (mean_fg_pct * padding) ) /
              (max(career_fg_attempt) + padding),
            .groups = 'drop') %>%
  distinct()
# -------------------------------------------------------------------------
# Career FG% by Career FG Attempt Count for Chosen Kickers ----------------
more_than_120 = fg_data %>%
  group_by(kicker_player_name) %>%
  summarise(max_attempts = max(career_fg_attempt), .groups = 'drop') %>%
  filter(max_attempts >= results$attempts) %>%
  pull(kicker_player_name)

chosen_kickers = fg_data %>%
  filter(kicker_player_name %in% more_than_120, career_fg_attempt %% 10 == 0)
chosen_kickers_career_plot = plot_ly(data = chosen_kickers,
                                  x = ~career_fg_attempt,
                                  y = ~career_fg_pct,
                                  color = ~kicker_player_name,
                                  colors = 'Paired',
                                  type = 'scatter',
                                  mode = 'lines',
                                  hoverinfo = 'text',
                                  text = ~paste0(
                                    'Kicker: ', kicker_player_name,
                                    '<br>Attempt: ', career_fg_attempt,
                                    '<br>FG%: ',
                                    sprintf('%.2f', career_fg_pct*100), '%'
                                  ),
                                  line = list(width = 0.5)) %>%
  layout(title = 'Career FG% Over Time for Chosen Kickers',
         xaxis = list(title = 'Career FG Attempt Count'),
         yaxis = list(title = 'Career FG%',
                      tickformat = '.1%'),
         showlegend = FALSE)
chosen_kickers_career_plot
# -------------------------------------------------------------------------
# Saving Data -------------------------------------------------------------
chosen_kickers = fg_data %>%
  group_by(kicker_player_id) %>%
  filter(max(career_fg_attempt) >= results$attempts) %>%
  ungroup()

filename = './data/cleaned/chosen_kickers.rdata'
save(chosen_kickers, file = filename)
# -------------------------------------------------------------------------