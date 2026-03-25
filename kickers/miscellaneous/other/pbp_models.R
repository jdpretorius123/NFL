glm_data = pbp_pp %>%
  dplyr::select(kick_distance, fg_result_clean, season, roof_clean) %>%
  mutate(
    fgr = factor(fg_result_clean, levels = c('missed', 'made')),
    roof = factor(roof_clean, levels = c('outdoors', 'indoors')),
    season = season - min(season)
  )
fgr_null_mod = glm(fgr ~ season, data = glm_data, family = binomial)
fgr_prtl_mod = glm(fgr ~ roof + season, data = glm_data, family = binomial)
fgr_full_mod = glm(
  fgr ~ roof + season + roof*season, 
  data = glm_data, 
  family = binomial
)

lrt_prtl = anova(fgr_null_mod, fgr_prtl_mod, test = 'LRT') %>%
  tidy() %>%
  filter(!is.na(p.value)) %>%
  pull(p.value)

lrt_full = anova(fgr_prtl_mod, fgr_full_mod, test = 'LRT') %>%
  tidy() %>%
  filter(!is.na(p.value)) %>%
  pull(p.value)

lrt_text = paste0(
  '**Likelihood Ratio Tests:**<br>',
  '**Partial vs Null (Roof Effect):** p = ', format.pval(lrt_prtl, eps = 0.001),
  '<br>',
  '**Full vs Partial (Interaction):** p = ', format.pval(lrt_full, eps = 0.001)
)

models_list = list(
  'Null (Season Effect)' = fgr_null_mod,
  'Partial (No Interaction)' = fgr_prtl_mod,
  'Full (Interaction)' = fgr_full_mod
)

all_glm_tidy = map_dfr(
  models_list, ~tidy(.x, conf.int = TRUE), .id = 'model_name'
) %>%
  mutate(
    across(c(estimate, conf.low, conf.high), exp),
    term = case_match(
      term,
      '(Intercept)' ~ 'Baseline (Outside)',
      'roofindoors' ~ 'Inside Effect',
      'season' ~ 'Yearly Trend (Yds/Year)',
      'roofindoors:season' ~ 'Interacton'
    ),
    display_val = paste0(
      round(estimate, 3),
      ' (p=',
      format.pval(p.value, digits = 3, eps = 0.01),
      ')'
    )
  )

glm_table_data = all_glm_tidy %>%
  dplyr::select(model_name, term, display_val) %>%
  pivot_wider(names_from = model_name, values_from = display_val) %>%
  replace(is.na(.), '-')

footnote_text = paste0(
  '**Note:**<br>',
  'Values are Odds Ratios with p-values in parentheses. ',
  'An OR > 1 indicates increased odds of a successful FG attempt. ',
  'ORs and CIs are exponentiated.'
)

glm_table_data %>%
  gt() %>%
  tab_header(
    title = md('<b>Model Comparison: The Odds of FG Conversion</b>'),
    subtitle = 'The Odds Keep Growing!'
  ) %>%
  tab_style(
    style = cell_text(weight = 'bold'),
    locations = cells_column_labels()
  ) %>%
  cols_label(term = 'Predictor') %>%
  tab_footnote(footnote = md(footnote_text)) %>%
  tab_source_note(source_note = md(lrt_text)) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(10)
  )

kck_dst_null_mod = lm(kick_distance ~ season, data = glm_data)
kck_dst_prtl_mod = lm(kick_distance ~ season + roof, data = glm_data)
kck_dst_full_mod = lm(
  kick_distance ~ season + roof + season*roof,
  data = glm_data
)

ftest_prtl = anova(kck_dst_null_mod, kck_dst_prtl_mod, test = 'F') %>%
  tidy() %>%
  filter(!is.na(p.value)) %>%
  pull(p.value)

ftest_full = anova(kck_dst_prtl_mod, kck_dst_full_mod, test = 'F') %>%
  tidy() %>%
  filter(!is.na(p.value)) %>%
  pull(p.value)

ftest_text = paste0(
  '**F-Tests:**<br>',
  '**Partial vs Null (Roof Effect):** p = ', format.pval(ftest_prtl, eps = 0.001),
  '<br>',
  '**Full vs Partial (Interaction):** p = ', format.pval(ftest_full, eps = 0.001)
)

models_list = list(
  'Null (Season Effect)' = kck_dst_null_mod,
  'Partial (No Interaction)' = kck_dst_prtl_mod,
  'Full (Interaction)' = kck_dst_full_mod
)

all_tidy = map_dfr(
  models_list, ~tidy(.x, conf.int = TRUE), .id = 'model_name'
) %>%
  mutate(
    term = case_match(
      term,
      '(Intercept)' ~ 'Baseline (Outside)',
      'roofindoors' ~ 'Inside Effect',
      'season' ~ 'Yearly Trend (Yds/Year)',
      'season:roofindoors' ~ 'Interacton'
    ),
    display_val = paste0(
      round(estimate, 3),
      ' (p=',
      format.pval(p.value, digits = 3, eps = 0.01),
      ')'
    )
  )

table_data = all_tidy %>%
  dplyr::select(model_name, term, display_val) %>%
  pivot_wider(names_from = model_name, values_from = display_val) %>%
  replace(is.na(.), '-')

footnote_text = paste0(
  '**Note:**<br>',
  'Estimates shown with p-values in parentheses. ',
  "The high p-value for the 'Interaction'",
  'suggests it does not significantly improve the model.'
)
table_data %>%
  gt() %>%
  tab_header(
    title = md('<b>Model Comparison: The Increasing Trend of Kick Distance</b>'),
    subtitle = 'Kickers Will Woon Be Attempting 80-yard Kicks'
  ) %>%
  tab_style(
    style = cell_text(weight = 'bold'),
    locations = cells_column_labels()
  ) %>%
  cols_label(term = 'Predictor') %>%
  tab_footnote(footnote = md(footnote_text)) %>%
  tab_source_note(source_note = md(ftest_text)) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(10)
  )
