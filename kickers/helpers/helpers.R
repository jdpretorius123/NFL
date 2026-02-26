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
# Dynamic Violin ----------------------------------------------------------
dynamic_violin = function(df, x_cols, y_cols) {
  #' Create grouped violin charts using a dataset (df), its numeric columns, and
  #' a grouping variable (x)
  #'
  #' @param df Dataset
  #' @param x_cols (character): Vector naming grouping variables of interest
  #' @param y_cols (character): Vector naming numeric variables of interest
  #' @return p Interactive plotly violin chart with a dropdown menu for variable
  #'  selection
  
  init_x = x_cols[1] 
  init_y = y_cols[1]
  
  p = plot_ly(
    data = df,
    x = ~get(init_x),
    y = ~get(init_y),
    type = 'violin',
    points = FALSE,
    meanline = list(visible = TRUE)
  )
  
  y_buttons = lapply(y_cols, function(col) {
    y_label = create_label(col)
    list (
      method = 'update',
      args = list(
        list(y = list(df[[col]])),
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
        list(x = list(df[[col]])),
        list(xaxis = list(title = x_label))
      ),
      label = x_label
    )
  })
  
  p %>% layout(
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
  
  p = plot_ly(
    data = df,
    x = ~get(init_x),
    y = ~get(init_y),
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 10, opacity = 0.5),
    hovertemplate = paste0(
      create_label(init_x), ': %{x}<br>',
      create_label(init_y), ': %{y}<extra></extra>'
    )
  )
  
  y_buttons = lapply(y_cols, function(col) {
    y_label = create_label(col)
    list(
      method = 'update',
      args = list(
        list(y = list(df[[col]])),
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
        list(x = list(df[[col]])),
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