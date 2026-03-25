# Create Label ------------------------------------------------------------
create_label = function(char_str) {
  #' Create a visually pleasing axis title using a character string
  #' (char_str)
  #' 
  #' @param char_str (character): Character string to convert into an axis
  #'  title
  #' @return axis_title (character): Visually pleasing axis title
  axis_title = stringr::str_to_title(
    stringr::str_replace_all(char_str, pattern = '_', replacement = ' ')
  )
  return(axis_title)
}
# -------------------------------------------------------------------------
# Grouped NA DF -----------------------------------------------------------
grouped_na_df = function(df, cols, group_var, col_prefix) {
  #' Create a dataset that summarizes the NA count in the variables (cols)
  #' across the levels of a grouping variable (group_var) in the given dataset
  #' (df).
  #' 
  #' @param df A dataset
  #' @param cols (character): Vector of variables in df to summarize NA count
  #' @param group_var (character): Character string naming the group variable
  #' @param col_prefix (character): Character string naming the column prefix
  #'  for pivot_wider
  #' @return na_df Dataset summarizing the NA count in cols across the 
  #'  levels of group_var
  
  gvar_symbol = rlang::sym(group_var)
  na_df = df %>%
    dplyr::select(all_of(c(group_var, cols))) %>%
    group_by(!!gvar_symbol) %>%
    summarise(across(dplyr::all_of(cols), ~sum( is.na(.) )),
              .groups = 'drop') %>%
    pivot_longer(cols = -!!gvar_symbol,
                 names_to = 'variable',
                 values_to = 'na_count') %>%
    pivot_wider(names_from = !!gvar_symbol,
                names_prefix = col_prefix,
                values_from = 'na_count')
  return(na_df)
}
# -------------------------------------------------------------------------
# Sumstats ----------------------------------------------------------------
sumstats = function(df, cols, summary_funcs) {
  #' Apply summary stats to the variables (cols) of the data frame (df).
  #' 
  #' @param df A dataset
  #' @param cols (character): Vector of variables in df to summarize
  #' @param summary_funcs (list): List of summary functions to apply to df
  #' @return sumstats (Dataset): Results of applying the summary_funcs across
  #'  the cols of df
  sumstats = df %>%
    dplyr::select(all_of(cols)) %>%
    summarise(
      across(
        .cols = dplyr::all_of(cols),
        .fns = summary_funcs
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c('.value', 'summary_stat'),
      names_pattern = '(.*)_(.*)'
    ) %>%
    mutate(across(where(is.numeric), ~sprintf('%0.2f', .)))
  return(sumstats)
}
# -------------------------------------------------------------------------
# Grouped Sumstats --------------------------------------------------------
grouped_sumstats = function(df, cols, group_var, summary_funcs) {
  #' Apply summary stats to the variables (cols) of the data frame (df),
  #' across the levels of a grouping variable (group_var).
  #' 
  #' @param df A dataset
  #' @param cols (character): Vector of variables in df to summarize
  #' @param group_var (character): Character string naming the group variable
  #' @param summary_funcs (list): List of summary functions to apply to df
  #' @return sumstats (Dataset): Results of applying the summary_funcs across
  #'  the cols of df across the levels of group_var
  gvar_symbol = rlang::sym(group_var)
  sumstats = df %>%
    dplyr::select(all_of(c(group_var, cols))) %>%
    group_by(!!gvar_symbol) %>%
    summarise(across(.cols = dplyr::all_of(cols),
                     .fns = summary_funcs,
                     .names = '{.col}_{.fn}'),
              .groups = 'drop') %>%
    pivot_longer(cols = -!!gvar_symbol,
                 names_to = c('variable', 'summary_stat'),
                 names_pattern = '(.*)_(.*)',
                 values_to = 'value') %>%
    pivot_wider(names_from = summary_stat,
                values_from = value)
  return(sumstats)
}
# -------------------------------------------------------------------------
# Histograms --------------------------------------------------------------
histograms = function(df, nbins, x_cols) {
  #' Create histograms using a dataset (df) and its numeric columns
  #' 
  #' @param df Numeric dataset 
  #' @param nbins (numeric): Number of bins to use for the histograms
  #' @param x_cols (characer): Vector of variable names
  #' @return p Interactive plotly histogram with a dropdown menu for variable
  #'  selection
  
  init_x = x_cols[1]
  
  p = plot_ly(
    data = df,
    x = ~get(init_x),
    type = 'histogram',
    nbinsx = nbins,
    marker = list(
      color = 'lightgrey',
      line = list(color = 'black', width = 1)
    ),
    showlegend = FALSE
  )
  
  buttons = lapply(x_cols, function(col) {
    x_label = create_label(col)
    list(
      method = 'update',
      args = list(
        list(x = list(df[[col]])),
        list(xaxis = list(title = x_label))
      ),
      label = x_label
    )
  })
  
  p %>% 
    layout(
      xaxis = list(title = create_label(init_x)),
      yaxis = list(title = 'Frequency'),
      margin = list(r = 150),
      updatemenus = list(
        list(
          buttons = buttons,
          x = 1.05, y = 0.6,
          xanchor = 'left', yanchor = 'top'
        )
      )
    ) %>%
    add_annotations(
      text = '<b>Select X:</b>',
      x = 1.05, y = 0.65,
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    )
}
# -------------------------------------------------------------------------
# Dynamic Strip Chart ----------------------------------------------------------
dynamic_strip_chart = function(df, y_cols) {
  #' Create grouped strip charts using a dataset (df) and its numeric columns
  #'
  #' @param df Dataset
  #' @param y_cols (character): Vector naming numeric variables of interest
  #' @return p Interactive plotly strip chart with a dropdown menu for variable
  #'  selection
  temp = df
  
  init_x = 'season'
  init_y = y_cols[1]
  
  init_x_label = create_label(init_x)
  init_y_label = create_label(init_y)
  
  temp$jit_season = temp[[init_x]] + runif(nrow(df), -0.3, 0.3)
  
  p = plot_ly(
    data = temp,
    x = ~jit_season,
    y = ~get(init_y),
    type = 'scattergl',
    mode = 'markers',
    
    marker = list(
      color = 'black',
      size = 5,
      opacity = 0.5
    ),
    
    customdata = ~team,
    hovertemplate = paste0(
      '<br>%{customdata}</b><br>',
      init_x_label, ': %{text}<br>',
      init_y_label, ': %{y:.2f}<extra></extra>'
    ),
    text = ~season
  )
  
  y_buttons = lapply(y_cols, function(col) {
    y_label = create_label(col)
    list (
      method = 'update',
      args = list(
        list(
          y = list(df[[col]]),
          hovertemplate = paste0(
            '<br>%{customdata}</b><br>',
            init_x_label, ': %{text}<br>',
            y_label, ': %{y:.2f}<extra></extra>'
          )
        ),
        list(yaxis = list(title = y_label))
      ),
      label = y_label
    )
  })
  
  p %>% layout(
    hovermode = 'closest',
    xaxis = list(
      title = init_x_label,
      tickmode = 'linear',
      dtick = 1,
      range = c(min(temp$season) - 1, max(temp$season) + 1)
    ),
    yaxis = list(title = init_y_label),
    showlegend = FALSE,
    margin = list(r = 150),
    updatemenus = list(
      list(
        buttons = y_buttons,
        x = 1.05, y = 0.6,
        xanchor = 'left', yanchor = 'top'
      )
    )
  ) %>%
    add_annotations(
      text = '<b>Select Y:</b>',
      x = 1.05, y = 0.65,
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    )
}
# -------------------------------------------------------------------------
# Dynamic Scatter Plot ----------------------------------------------------
dynamic_scatter_plot = function(df, x_cols, y_cols) {
  #' Create scatter plots using a dataset (df) and any numeric cols (x_cols and
  #' y_cols) that you fancy.
  #'
  #' @param df Dataset
  #' @param x_cols (character): Vector naming x-axis variables of interest
  #' @param y_cols (character): Vector naming y-axis variables of interest
  #' @return p Interactive plotly scatter plot with a dropdown menu for variable
  #'  selection
  init_x = x_cols[1]
  init_y = y_cols[1]
  
  init_x_label = create_label(init_x)
  init_y_label = create_label(init_y)
  
  p = plot_ly(
    data = df,
    x = ~get(init_x),
    y = ~get(init_y),
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 10, opacity = 0.5),
    hovertemplate = paste0(
      init_x_label, ': %{x}<br>',
      init_y_label, ': %{y}<extra></extra>'
    )
  )
  
  y_buttons = lapply(y_cols, function(col) {
    y_label = create_label(col)
    list(
      method = 'update',
      args = list(
        list(
          y = list(df[[col]]),
          hovertemplate = paste0(
            init_x_label, ':%{x}<br>',
            y_label, ':%{y}<extra></extra>'
          )
        ),
        list(yaxis = list(title = y_label))
      ),
      label = y_label
    )
  })
  
  x_buttons = lapply(x_cols, function(col) {
    x_label = create_label(col)
    list(
      method = 'update',
      args = list(
        list(
          x = list(df[[col]]),
          hovertemplate = paste0(
            x_label, ':%{x}<br>',
            init_y_label, ':%{y}<extra></extra>'
          )
        ),
        list(xaxis = list(title = x_label))
      ),
      label = x_label
    )
  })
  
  p %>%
    layout(
      xaxis = list(title = create_label(init_x)),
      yaxis = list(title = create_label(init_y)),
      showlegend = FALSE,
      margin = list(r = 150),
      updatemenus = list(
        list(
          buttons = y_buttons,
          x = 1.05, y = 0.8,
          xanchor = 'left', yanchor = 'top'
        ),
        list(
          buttons = x_buttons,
          x = 1.05, y = 0.5,
          xanchor = 'left', yanchor = 'top'
        )
      )
    ) %>%
    add_annotations(
      text = '<b>Select Y:</b>',
      x = 1.05, y = 0.85,
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    ) %>%
    add_annotations(
      text = '<b>Select X:</b>',
      x = 1.05, y = 0.55,
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    )
}
# -------------------------------------------------------------------------
# Download Link -----------------------------------------------------------
download_link = function(data, filename = 'pbp_endpoints.csv') {
  #' Tabulate a dataset (data) and specify the name of its exported file.
  #'
  #' @param data Dataset
  #' @param filename (character): String naming the export file
  csv_data = readr::format_csv(data)
  encoded_data = base64encode( charToRaw(csv_data) )
  tags$a(
    'Download CSV',
    href = paste0('data:text/csv;base64,', encoded_data),
    download = filename,
    class = 'button'
  )
}
# -------------------------------------------------------------------------
# Dynamic Line Plot -----------------------------------------------------
dynamic_line_plot = function(df, x_cols, y_cols) {
  #' Create line plots using a dataset (df) and any time vars (x_cols) and
  #' numeric cols (y_cols) that you fancy.
  #'
  #' @param df Dataset
  #' @param x_cols (character): Vector naming x-axis time variables of interest
  #' @param y_cols (character): Vector naming y-axis variables of interest
  #' @return p Interactive plotly line plot with a dropdown menu for variable
  #'  selection
  init_x = x_cols[1]
  init_y = y_cols[1]
  
  init_x_label = create_label(init_x)
  init_y_label = create_label(init_y)
  
  p = plot_ly(
    data = df,
    x = ~get(init_x),
    y = ~get(init_y),
    type = 'scatter',
    mode = 'lines+markers',
    marker = list(size = 10, opacity = 0.5),
    hovertemplate = paste0(
      init_x_label, ': %{x}<br>',
      init_y_label, ': %{y}<extra></extra>'
    )
  )
  
  y_buttons = lapply(y_cols, function(col) {
    y_label = create_label(col)
    list(
      method = 'update',
      args = list(
        list(
          y = list(df[[col]]),
          hovertemplate = paste0(
            init_x_label, ':%{x}<br>',
            y_label, ':%{y}<extra></extra>'
          )
        ),
        list(yaxis = list(title = y_label))
      ),
      label = y_label
    )
  })
  
  x_buttons = lapply(x_cols, function(col) {
    x_label = create_label(col)
    list(
      method = 'update',
      args = list(
        list(
          x = list(df[[col]]),
          hovertemplate = paste0(
            x_label, ':%{x}<br>',
            init_y_label, ':%{y}<extra></extra>'
          )
        ),
        list(xaxis = list(title = x_label))
      ),
      label = x_label
    )
  })
  
  p %>%
    layout(
      xaxis = list(title = create_label(init_x)),
      yaxis = list(title = create_label(init_y)),
      showlegend = FALSE,
      margin = list(r = 150),
      updatemenus = list(
        list(
          buttons = y_buttons,
          x = 1.05, y = 0.8,
          xanchor = 'left', yanchor = 'top'
        ),
        list(
          buttons = x_buttons,
          x = 1.05, y = 0.5,
          xanchor = 'left', yanchor = 'top'
        )
      )
    ) %>%
    add_annotations(
      text = '<b>Select Y:</b>',
      x = 1.05, y = 0.85,
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    ) %>%
    add_annotations(
      text = '<b>Select X:</b>',
      x = 1.05, y = 0.55,
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    )
}
# -------------------------------------------------------------------------
# K-Means Threshold -------------------------------------------------------
kmeans_threshold = function(df, v, n, iter) {
  #' Split a dataset (df) into clusters (n) using a variable (v) of choice
  #' 
  #' @param df Dataset
  #' @param column (character): String naming the variable of choice
  #' @param n (integer): Integer specifying the number of clusters to create
  #' @param iter (integer): Integer specifying the number of kmeans iterations 
  #'  to run to determine the best fit for the data
  #' @return threshold (integer): Integer representing the average of n
  #'  cluster centers
  km = kmeans(df[[v]], centers = n, nstart = iter)
  
  centers = sort(km$centers[,1])
  threshold = mean(centers)
  
  return(threshold)
}
# -------------------------------------------------------------------------
# Dynamic Heat Map --------------------------------------------------------
dynamic_heatmap = function(df, x_col, metric_list) {
  #' Create a heat map that allows the user to assess lists of metrics
  #' (metric_list) within a dataframe (df) over time (x_col
  #' 
  #' @param df Dataset
  #' @param x_col (character): String naming a time variable of interest
  #' @param metric_list (list): A named list where each element is a vector of 
  #'  columns within df
  #' @return Interactive heat map with a dropdown menu for selecting multiple
  #'  columns within df
  z_matrices = lapply(metric_list, function(cols) {
    t(as.matrix(df[, cols]))
  })
  
  init_metric_name = names(metric_list)[1]
  init_y_cols = metric_list[[1]]
  init_y_labels = unname(sapply(init_y_cols, create_label))
  
  metric_buttons = lapply(names(metric_list), function(name) {
    current_y_labels = unname(sapply(metric_list[[name]], create_label))
    
    list(
      method = 'update',
      args = list(
        list(
          z = list(z_matrices[[name]]),
          y = list(current_y_labels) 
        ),
        list(
          yaxis = list(
            title = name, 
            autorange = 'reversed'
          )
        )
      ),
      label = name
    )
  })
  
  plot_ly(
    x = df[[x_col]],
    y = init_y_labels,
    z = z_matrices[[1]],
    type = 'heatmap',
    colorscale = 'Viridis',
    showscale = FALSE,
    hovertemplate = paste0(
      'Season: %{x}<br>',
      'Value: %{z:.2f}<extra></extra>'
    )
  ) %>%
    layout(
      xaxis = list(title = create_label(x_col), dtick = 1),
      yaxis = list(title = init_metric_name, autorange = 'reversed'),
      margin = list(l = 120, r = 150, b = 50, t = 50),
      updatemenus = list(
        list(
          buttons = metric_buttons,
          x = 1.05, y = 0.8,
          xanchor = 'left', yanchor = 'top'
        )
      )
    ) %>%
    add_annotations(
      text = '<b>Select Metric:</b>',
      x = 1.05, y = 0.85,
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    )
}
# -------------------------------------------------------------------------
# NFL Teams Line Plot -----------------------------------------------
nfl_teams_line_plot = function(df, y_cols) {
  #' Create line plots using a dataset (df) and the numeric cols (y_cols) that 
  #' you fancy.
  #'
  #' @param df Dataset
  #' @param y_cols (character): Vector naming y-axis variables of interest
  #' @return p Interactive plotly line plot with a dropdown menu for variable
  #'  selection
  df = df %>%
    arrange(season, team)
  
  init_y = y_cols[1]
  init_y_label = create_label(init_y)
  
  team_info = nflreadr::load_teams() %>%
    dplyr::select(team_abbr, team_color, team_color2)
  nfl_colors = team_info$team_color
  names(nfl_colors) = team_info$team_abbr
  
  teams = sort(unique(df[['team']]))
  n_teams = length(teams)
  
  init_vis = as.list(c(TRUE, rep(FALSE, n_teams - 1)))
  
  p = plot_ly(
    data = df,
    x = ~get('season'),
    y = ~get(init_y),
    color = ~get('team'),
    colors = nfl_colors,
    type = 'scattergl',
    mode = 'lines+markers',
    visible = init_vis,
    hovertemplate = paste0(
      '%{fullData.name}<br>',
      'Season: %{x}<br>',
      init_y_label, ': %{y}<extra></extra>'
    )
  ) %>%
    style(visible = FALSE, traces = 2:n_teams) %>%
    style(visible = TRUE, traces = 1)
  
  y_buttons = lapply(y_cols, function(col) {
    y_label = create_label(col)
    list(
      method = 'restyle',
      args = list( 
        list(
          y = rep( list(df[[col]]), n_teams),
          hovertemplate = paste0(
            '%{fullData.name}<br>',
            'Season: %{x}<br>',
            y_label, ': %{y}<extra></extra>'
          )
        )
      ),
      label = y_label
    )
  })
  
  team_buttons = lapply(seq_along(teams), function(i) {
    vis_vector = rep(FALSE, n_teams)
    vis_vector[i] = TRUE
    list(
      method = 'restyle',
      args = list('visible', as.list(vis_vector)),
      label = teams[i]
    )
  })
  
  p %>%
    layout(
      xaxis = list(title = 'Season', tickmode = 'linear', dtick = 1),
      yaxis = list(title = init_y_label),
      showlegend = FALSE,
      hovermode = 'closest',
      margin = list(r = 160, t = 50),
      updatemenus = list(
        list(
          buttons = team_buttons,
          x = 1.05, y = 0.8,
          xanchor = 'left', yanchor = 'top'
        ),
        list(
          buttons = y_buttons,
          x = 1.05, y = 0.5,
          xanchor = 'left', yanchor = 'top'
        )
      )
    ) %>%
    add_annotations(
      text = c('<b>Select Team:</b>', '<b>Select Y:</b>'),
      x = 1.05, y = c(0.85, 0.55),
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    )
}
# -------------------------------------------------------------------------
# NFL Teams Heat Map ------------------------------------------------------
nfl_teams_heatmap = function(df, metric_list) {
  #' Create a heat map that allows the user to assess lists of metrics
  #' (metric_list) within a dataframe (df)
  #' 
  #' @param df Dataset
  #' @param metric_list (list): A named list where each element is a vector of 
  #'  columns within df
  #' @return Interactive heat map with a dropdown menu for selecting multiple
  #'  columns within df
  seasons = sort(unique(df$season))
  teams = sort(unique(df$team))
  
  get_z_matrix = function(met) {
    mat_df = df %>%
      dplyr::select(all_of(c('team', 'season', met))) %>%
      pivot_wider(names_from = all_of('season'), values_from = all_of(met)) %>%
      arrange(get('team')) %>%
      select(-all_of('team'))
    
    return(as.matrix(mat_df))
  }
  
  init_metric = metric_list[1]
  init_z = get_z_matrix(init_metric)
  
  p = plot_ly(
    x = seasons,
    y = teams,
    z = init_z,
    type = 'heatmap',
    colorscale = 'Viridis',
    reversescale = FALSE,
    showscale = FALSE,
    hovertemplate = paste0(
      'Team: %{y}<br>',
      'Season: %{x}<br>',
      'Value: %{z:.2f}<extra></extra>'
    )
  )
  
  metric_buttons = lapply(metric_list, function(met) {
    met_label = create_label(met)
    list(
      method = 'restyle',
      args = list(
        list(
          z = list(get_z_matrix(met)),
          hovertemplate = paste0(
            'Team: %{y}<br>',
            'Season: %{x}<br>',
            met_label, ': %{z:.2f}<extra></extra>'
          )
        )
      ),
      label = met_label
    )
  })
  
  p %>%
    layout(
      xaxis = list(title = 'Season', dtick = 1, side = 'bottom'),
      yaxis = list(title = '', autorange = 'reversed'),
      margin = list(l = 150, r = 50, b = 50, t = 50),
      updatemenus = list(
        list(
          buttons = metric_buttons,
          x = 1.05, y = 0.8,
          xanchor = 'left', yanchor = 'top'
        )
      )
    ) %>%
    add_annotations(
      text = '<b>Select Metric:</b>',
      x = 1.05, y = 0.85,
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    )
}
# -------------------------------------------------------------------------
# NFL Teams Scatter Plot --------------------------------------------------
nfl_teams_scatter_plot = function(df, x_cols, y_cols) {
  #' Create scatter plots using a dataset (df) and any numeric cols (x_cols and
  #' y_cols) that you fancy.
  #'
  #' @param df Dataset
  #' @param x_cols (character): Vector naming x-axis variables of interest
  #' @param y_cols (character): Vector naming y-axis variables of interest
  #' @return p Interactive plotly scatter plot with a dropdown menu for variable
  #'  selection
  init_x = 'total_fgm'
  init_y = 'mean_trvl_dst_mtrs'
  init_x_label = create_label(init_x)
  init_y_label = create_label(init_y)
  
  team_info = nflreadr::load_teams() %>%
    dplyr::select(team_abbr, team_color)
  nfl_colors = team_info$team_color
  names(nfl_colors) = team_info$team_abbr
  
  p = plot_ly(
    data = df,
    x = ~get(init_x),
    y = ~get(init_y),
    color = ~team,      
    colors = nfl_colors, 
    type = 'scattergl', 
    mode = 'markers',
    marker = list(
      size = 10, 
      opacity = 0.8, 
      line = list(color = '#FFFFFF', width = 1) 
    ),
    text = ~team,
    hovertemplate = paste0(
      '%{text}<br>',
      init_x_label, ': %{x}<br>',
      init_y_label, ': %{y}<extra></extra>'
    )
  )
  
  y_buttons = lapply(y_cols, function(col) {
    y_label = create_label(col)
    list(
      method = 'update',
      args = list(
        list(
          y = list(df[[col]]),
          hovertemplate = paste0(
            '%{text}<br>',
            init_x_label, ': %{x}<br>',
            y_label, ': %{y}<extra></extra>'
          )
        ),
        list(yaxis = list(title = y_label))
      ),
      label = y_label
    )
  })
  
  x_buttons = lapply(x_cols, function(col) {
    x_label = create_label(col)
    list(
      method = 'update',
      args = list(
        list(
          x = list(df[[col]]),
          hovertemplate = paste0(
            '%{text}<br>',
            x_label, ': %{x}<br>',
            init_y_label, ': %{y}<extra></extra>'
          )
        ),
        list(xaxis = list(title = x_label))
      ),
      label = x_label
    )
  })
  
  p %>%
    layout(
      xaxis = list(title = init_x_label),
      yaxis = list(title = init_y_label),
      showlegend = FALSE, 
      hovermode = 'closest',
      margin = list(r = 150),
      updatemenus = list(
        list(
          buttons = y_buttons,
          x = 1.05, y = 0.8,
          xanchor = 'left', yanchor = 'top'
        ),
        list(
          buttons = x_buttons,
          x = 1.05, y = 0.5,
          xanchor = 'left', yanchor = 'top'
        )
      )
    ) %>%
    add_annotations(
      text = c('<b>Select Y:</b>', '<b>Select X:</b>'),
      x = 1.05, y = c(0.85, 0.55),
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    )
}
# -------------------------------------------------------------------------
# Grouped Line Plot -------------------------------------------------------
grouped_line_plot = function(df, x_col, y_cols, group_col) {
  #' Create a grouped line plot
  #'
  #' @param df Dataset
  #' @param x_col (character): The time variable
  #' @param y_cols (character): Numeric metrics
  #' @param group_col (character): The category variable
  
  df = df %>% 
    dplyr::arrange(!!rlang::sym(group_col), !!rlang::sym(x_col))
  df[[group_col]] = as.factor(df[[group_col]])
  
  groups = levels(df[[group_col]])
  n_groups = length(groups)
  
  init_y = y_cols[1]
  init_y_label = create_label(init_y)
  
  p = plot_ly()
  
  for (g in groups) {
    group_data = df[df[[group_col]] == g, ]
    p = p %>% add_trace(
      data = group_data,
      x = ~get(x_col),
      y = ~get(init_y),
      name = g,
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(size = 8),
      hovertemplate = paste0(
        '<b>', g, '</b><br>',
        'Season: %{x}<br>',
        init_y_label, ': %{y:.2f}<extra></extra>'
      )
    )
  }
  
  y_buttons = lapply(y_cols, function(col) {
    y_label = create_label(col)
    new_y_data = unname(split(df[[col]], df[[group_col]]))
    new_hovers = lapply(groups, function(g) {
      paste0(
        '<b>', g, '</b><br>', 
        'Season: %{x}<br>', 
        y_label, ': %{y:.2f}<extra></extra>'
      )
    })
    
    list(
      method = 'update',
      args = list(
        list(
          y = new_y_data,
          hovertemplate = new_hovers
        ),
        list(yaxis = list(title = y_label))
      ),
      label = y_label
    )
  })
  
  p %>% layout(
    xaxis = list(title = 'Season', dtick = 1),
    yaxis = list(title = init_y_label),
    hovermode = 'closest',
    showlegend = TRUE,
    legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
    margin = list(r = 160, t = 50),
    updatemenus = list(
      list(
        buttons = y_buttons,
        x = 1.05, y = 0.8,
        xanchor = 'left', yanchor = 'top'
      )
    )
  ) %>%
    add_annotations(
      text = '<b>Select Metric:</b>',
      x = 1.05, y = 0.85,
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    )
}
# -------------------------------------------------------------------------

# Get Colors --------------------------------------------------------------
get_colors = function(chg_vec) {
  #' Associate colors with a vector of values indicating change 
  #'
  #' @param chg_vec (number): Numeric vector of values indicating change
  ifelse(chg_vec >= 0, "#28a745", "#dc3545")
}
# -------------------------------------------------------------------------
# Stadium Impact Plot -----------------------------------------------------
stadium_impact_plot = function(df, chg_cols, group_col) {
  #' Create a bar plot to visualize the impact of change
  #'
  #' @param df Dataset
  #' @param chg_cols (character): Character vector of columns indicating change
  #' @param group_col (character): String indicating variable to group by
  plot_df = df %>% 
    filter(is_current == TRUE) %>%
    arrange(home_team)
  
  init_met = chg_cols[1]
  init_met_label = create_label(init_met)
  
  p = plot_ly(data = plot_df) %>%
    add_trace(
      x = ~get(init_met),
      y = ~get(group_col),
      type = 'bar',
      orientation = 'h',
      marker = list(
        color = get_colors(plot_df[[init_met]]),
        line = list(color = 'white', width = 1)
      ),
      customdata = ~roof_chg, 
      hovertemplate = paste0(
        '%{y}<br>',
        'Transition: %{customdata}<br>',
        'Impact: %{x:.2f}<extra></extra>'
      )
    )
  
  buttons = lapply(chg_cols, function(met) {
    met_label = create_label(met)
    current_vec = plot_df[[met]]
    
    list(
      method = 'update',
      args = list(
        list(
          x = list(current_vec),
          marker = list(
            color = get_colors(current_vec),
            line = list(color = 'white', width = 1)
          ),
          hovertemplate = paste0(
            '%{y}<br>Transition: %{customdata}<br>',
            met_label, ': %{x:.2f}<extra></extra>'
          )
        ),
        list(xaxis = list(title = paste('Change in', met_label)))
      ),
      label = met_label
    )
  })
  
  p %>% layout(
    xaxis = list(
      title = paste('Change in', init_met_label),
      zeroline = TRUE, 
      zerolinewidth = 2, 
      zerolinecolor = '#333'
    ),
    yaxis = list(
      title = '', 
      autorange = 'reversed'
    ),
    margin = list(l = 100, r = 150, t = 80),
    updatemenus = list(
      list(
        buttons = buttons,
        x = 1.05, y = 0.8,
        xanchor = 'left', yanchor = 'top'
      )
    )
  ) %>%
    add_annotations(
      text = '<b>Select Metric:</b>',
      x = 1.05, y = 0.85,
      xref = 'paper', yref = 'paper',
      xanchor = 'left', yanchor = 'bottom',
      showarrow = FALSE
    )
}
# -------------------------------------------------------------------------


