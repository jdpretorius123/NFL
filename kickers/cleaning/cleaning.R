#!/usr/bin/env Rscript --vanilla

# Loading Dependencies ----------------------------------------------------

packages = c('tidyverse', 'lubridate', 'here', 'zoo')

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

pbp_file = here('kickers', 'data', 'raw', 'pbp', 'pbp.rdata')
load(pbp_file)

stats_file = here('kickers', 'data', 'raw', 'stats', 'stats.rdata')
load(stats_file)

# -------------------------------------------------------------------------
# Cleaning PBP ------------------------------------------------------------

grass = c('grass', 'grass ')
turf = c('a_turf', 'astroplay', 'astroturf', 'dessograss', 'fieldturf',
         'matrixturf', 'sportturf')

datetime_format = '%m/%d/%y, %H:%M:%S' 
time_format = '%H:%M:%S'
timezone = 'America/New_York'

bad_weather = paste(
  'rain', 'rainy', 'raining',
  'snow', 'snowing', 'snow flurries',
  'showers', 'storms', 'thunderstroms',
  sep = '|'
)
decent_weather = paste(
  'cloudy', 'clouds', 'overcast', 
  'fog', 'humid', 'damp',
  'cold', 'hot',
  sep = '|'
)
good_weather = paste(
  'sunny', 'sun', 
  'clear', 'clear skies', 'few clouds',
  'beautiful', 'fair',
  sep = '|'
)

outdoor_games = pbp %>%
  filter(roof == 'outdoors') %>%
  dplyr::select(game_id, temp, wind) %>%
  distinct()

medians = outdoor_games %>%
  mutate(
    temp_median = median(temp, na.rm = TRUE),
    wind_median = median(wind, na.rm = TRUE)
  ) %>%
  dplyr::select(temp_median, wind_median) %>%
  distinct()

z_scores = outdoor_games %>%
  mutate(
    temp_z_score = (temp - mean(temp, na.rm = TRUE)) / sd(temp, na.rm = TRUE),
    wind_z_score = (wind - mean(wind, na.rm = TRUE)) / sd(wind, na.rm = TRUE)
  )

temp_pbp = pbp %>%
  mutate(
    start_time_clean = str_replace(start_time, '24:', '10:'),
    
    start_time_clean = case_when(
      grepl('/', start_time_clean, fixed = TRUE) ~ 
        as.POSIXct(start_time_clean, format = datetime_format , tz = timezone),
      TRUE ~ as.POSIXct(start_time_clean, format = time_format, tz = timezone)),
    
    start_time_clean = hms::as_hms(start_time_clean),
    start_time_clean = case_when(
      hour(start_time_clean) <= 13 ~ 'morning',
      hour(start_time_clean) > 13 & hour(start_time_clean) <= 17 ~ 'afternoon',
      hour(start_time_clean) > 17 ~ 'night',
      .default = NA
    ),
    
    fg_result_clean = case_when(
      is.na(field_goal_result) ~ NA,
      field_goal_result == 'made' ~ 'made',
      .default = 'missed'
      ),
    
    roof_clean = if_else(
      roof == 'closed' | roof == 'dome', 'indoors', 'outdoors'
    ),
    
    surface_clean = case_when(
     surface %in% grass ~ 'grass',
     surface %in% turf ~ 'turf',
     .default = NA
    ),
    
    weather_clean = str_remove(weather, "Temp:.*"),
    weather_clean = case_when(
      roof_clean == 'indoors' ~ 'indoors',
      grepl(bad_weather, weather_clean, ignore.case = TRUE) ~ 'bad',
      grepl(decent_weather, weather_clean, ignore.case = TRUE) ~ 'decent',
      grepl(good_weather, weather_clean, ignore.case = TRUE) ~ 'good',
      is.na(weather_clean) | weather_clean == '' ~ NA,
      .default = NA
    ),
    
    temp_clean = if_else(roof_clean == 'indoors', 70, temp),
    wind_clean = if_else(roof_clean == 'indoors', 0, wind), 
    
    kicker_name_clean = str_replace(
      kicker_player_name, pattern = '\\. ', replacement = '\\.'
    ),
    kicker_name_clean = case_when(
      kicker_name_clean == 'Gay' ~ 'J.Gay',
      kicker_name_clean == 'Je.Reed' ~ 'J.Reed',
      (kicker_name_clean == 'Josh.Brown' | kicker_name_clean == 'J.Brown') & 
        kicker_player_id == '00-0021940' ~ 'Josh Brown',
      .default = kicker_name_clean
    ),
    
    drive_top_clean = paste0('00:', drive_time_of_possession),
    drive_top_clean = hms::parse_hms(drive_top_clean),
    drive_top_clean = (minute(drive_top_clean)*60) +
      second(drive_top_clean),
    
    game_date = ymd(game_date)
  ) %>%
  filter(season > 2000, !(play_type %in% c('extra_point', 'no_play')))

temp_pbp = temp_pbp %>%
  group_by(season, stadium_id) %>%
  arrange(week) %>%
  mutate(
    temp_clean = if_else(
      roof_clean == 'outdoors',
      na.approx(temp_clean, na.rm = FALSE),
      temp_clean
    ),
    temp_clean = if_else(is.na(temp_clean), medians$temp_median, temp_clean),
    wind_clean = if_else(
      roof_clean == 'outdoors',
      na.approx(wind_clean, na.rm = FALSE),
      wind_clean
    ),
    wind_clean = if_else(is.na(wind_clean), medians$wind_median, wind_clean)
  ) %>%
  ungroup()

temp_pbp = temp_pbp %>%
  arrange(stadium_id, season, week) %>%
  group_by(stadium_id) %>%
  fill(surface_clean, .direction = 'down') %>%
  fill(weather_clean, .direction = 'down') %>%
  ungroup() %>%
  mutate(surface_clean = if_else(is.na(surface_clean), 'grass', surface_clean))

pbp_clean = temp_pbp %>%
  left_join(
    y = z_scores %>%
      dplyr::select(game_id, temp_z_score, wind_z_score),
    by = 'game_id'
  ) %>%
  mutate(
    weather_clean = if_else(
      is.na(weather_clean),
      case_when(
        abs(temp_z_score) > 2 | wind_z_score > 2 ~ 'bad',
        abs(temp_z_score) <= 1 & wind_z_score <= 1 ~ 'good',
        .default = 'decent'),
      weather_clean
    )
  ) %>%
  dplyr::select(-temp_z_score, -wind_z_score)

# -------------------------------------------------------------------------
# Cleaning Stats ----------------------------------------------------------

stats_clean = stats %>%
  filter(season > 2000)

# -------------------------------------------------------------------------
# Saving the Cleaned Data -------------------------------------------------

pbp_clean_file = here('kickers', 'data', 'clean', 'pbp', 'pbp_clean.rdata')
stats_clean_file = here('kickers', 'data', 'clean', 'stats', 'stats_clean.rdata')

#' Replacing the old cleaned pbp data with new data
if (file.exists(pbp_clean_file)) {
  file.remove(pbp_clean_file)
  cat(basename(pbp_clean_file), 'has been deleted.\n')
} else {
  cat(basename(pbp_clean_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(pbp_clean, file = pbp_clean_file)
if (file.exists(pbp_clean_file)) {
  cat(basename(pbp_clean_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(pbp_clean_file), '\n')
}

#' Replacing the old cleaned stats data with new data
if (file.exists(stats_clean_file)) {
  file.remove(stats_clean_file)
  cat(basename(stats_clean_file), 'has been deleted.\n')
} else {
  cat(basename(stats_clean_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(stats_clean, file = stats_clean_file)
if (file.exists(stats_clean_file)) {
  cat(basename(stats_clean_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(stats_clean_file), '\n')
}

# -------------------------------------------------------------------------