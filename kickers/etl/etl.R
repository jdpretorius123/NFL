#!/usr/bin/env Rscript --vanilla

# Installing/Loading Dependencies -----------------------------------------

packages = c(
  'tidyverse',
  'lubridate',
  'nflfastR',
  'nflreadr',
  'here'
)

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
# Loading the Helper Functions --------------------------------------------

helpers_file = here('kickers', 'helpers', 'helpers.R')
source(helpers_file)

# -------------------------------------------------------------------------
# Processing Command Line Arguments  --------------------------------------

input_file = commandArgs(trailingOnly = TRUE)
if (length(input_file) == 0) {
  stop('No input file provided.', call. = FALSE)
}

usr_file = read_csv(input_file, show_col_types = FALSE)
position = unique(usr_file$position)
pbp_vars = na.omit(usr_file$pbp_vars)
demo_vars = na.omit(usr_file$demo_vars)
stats_vars = na.omit(usr_file$stats_vars)

# -------------------------------------------------------------------------
# Loading the Raw Data ----------------------------------------------------

nflreadr::clear_cache()
options(nflreadr.verbose = FALSE)

pbp = nflfastR::load_pbp(TRUE)
pbp = pbp %>%
  filter(!is.na(kicker_player_id), season_type == 'REG') %>%
  dplyr::select(all_of(pbp_vars))

demo = nflreadr::load_players()
demo = demo %>%
  filter(position == 'K', !is.na(demo$gsis_id)) %>%
  dplyr::select(all_of(demo_vars))

stats = nflfastR::load_player_stats(seasons = TRUE)
stats = stats %>%
  filter(position == 'K', !is.na(stats$player_id), season_type == 'REG') %>%
  dplyr::select(all_of(stats_vars))

# -------------------------------------------------------------------------
# Joining the Data --------------------------------------------------------

pbp = left_join(x = pbp, y = demo, by = c('kicker_player_id' = 'gsis_id')) %>%
  filter(!is.na(position))
stats = left_join(x = stats, 
                  y = demo, 
                  by = c('player_id' = 'gsis_id', 'position'))

# -------------------------------------------------------------------------
# Replacing Old Data with New Data ----------------------------------------

#' Defining the file names
pbp_data_file = here('kickers', 'data', 'raw', 'pbp', 'pbp.rdata')
stats_data_file = here('kickers', 'data', 'raw', 'stats', 'stats.rdata')

#' Replacing the old pbp data with new data
if (file.exists(pbp_data_file)) {
  file.remove(pbp_data_file)
  cat(basename(pbp_data_file), 'has been deleted.\n')
} else {
  cat(basename(pbp_data_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(pbp, file = pbp_data_file)
if (file.exists(pbp_data_file)) {
  cat(basename(pbp_data_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(pbp_data_file), '\n')
}

#' Replacing the old stats data with new data
if (file.exists(stats_data_file)) {
  file.remove(stats_data_file)
  cat(basename(stats_data_file), 'has been deleted.\n')
} else {
  cat(basename(stats_data_file), 'not found.\n')
}

cat('Saving a new copy...\n')
save(stats, file = stats_data_file)
if (file.exists(stats_data_file)) {
  cat(basename(stats_data_file), 'has been saved.\n')
} else {
  cat('Unable to save', basename(stats_data_file), '\n')
}

# -------------------------------------------------------------------------