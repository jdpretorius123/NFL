# Miscellaneous Code ------------------------------------------------------

team_info = load_teams() %>%
  dplyr::select(team_abbr, team_color, team_color2)
nfl_colors = team_info$team_color
names(nfl_colors) = team_info$team_abbr

plot_ly(
  data = team_stats,
  x = ~season,
  y = ~n_kickers,
  color = ~team,
  colors = nfl_colors,
  customdata = ~team,
  type = 'scatter',
  mode = 'lines+markers',
  hovertemplate = paste0(
    'NFL Team: %{customdata}<br>',
    'Number of Kickers: %{y}<extra></extra>'
  )
) %>%
  layout(
    title = 'NFL Team Kicker Count',
    xaxis = list(title = 'Season', dtick = 1),
    yaxis = list(title = 'Number of Kickers', rangemode = 'tozero'),
    showlegend = FALSE
  )

# -------------------------------------------------------------------------