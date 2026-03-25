library(shiny)
library(plotly)
library(dplyr)
library(bslib)
library(DT)

xfgoe_ldrbrd_file = 'xfgoe_ldrbrd_v1.rdata'

if (file.exists(xfgoe_ldrbrd_file)) {
  load(xfgoe_ldrbrd_file)
}

# --- UI Side ---
ui <- page_navbar(
  title = "NFL Kicker Analysis",
  theme = bs_theme(bootswatch = "flatly"),
  
  tags$head(uiOutput("dynamic_css")),
  
  # ==========================================
  # PAGE 1: The Player Profile
  # ==========================================
  nav_panel("Player Profile",
            
            # ROW 1: Selection (Fixed height, No scrollbars)
            layout_columns(
              fill = FALSE, 
              card(
                id = "nav_card",
                div(style = "display: flex; align-items: center; height: 100%; padding: 0 20px; overflow: visible;",
                    div("Select Player:", style = "font-weight: bold; font-size: 16px; margin-right: 15px; white-space: nowrap;"),
                    div(style = "width: 250px; z-index: 1001;", 
                        selectInput("sel_kicker", NULL, choices = sort(xfgoe_ldrbrd$kicker_name_clean), width = "100%")
                    )
                )
              ),
              col_widths = 12 
            ),
            
            # ROW 2: Bio & Spider Chart
            layout_columns(
              card(
                id = "bio_card",
                div(style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; padding: 10px;",
                    uiOutput("kicker_headshot"),
                    h2(textOutput("kicker_name"), style = "margin-top: 15px; margin-bottom: 0px; font-weight: bold;"),
                    p(textOutput("kicker_id"), style = "color: #6c757d; font-size: 14px;")
                )
              ),
              card(
                id = "plot_card",
                card_header("Environment & Performance Profile", style = "background-color: transparent; border-bottom: none; font-weight: bold;"),
                div(style = "padding: 10px; height: 100%; display: flex; align-items: center; justify-content: center;",
                    plotlyOutput("spider_chart", height = "450px")
                )
              ),
              col_widths = c(4, 8)
            )
  ),
  
  nav_panel("Full Leaderboard",
            card(
              card_header("xFGOE Leaderboard", class = "bg-primary text-white"),
              div(style = "padding: 10px;",
                  DTOutput("leaderboard_table")
              )
            )
  )
)

# --- Server Side ---
server <- function(input, output, session) {
  
  selected_data <- reactive({
    xfgoe_ldrbrd %>% filter(kicker_name_clean == input$sel_kicker)
  })
  
  output$dynamic_css <- renderUI({
    logo_url <- selected_data()$team_logo_espn
    css <- paste0("
      #nav_card, #bio_card, #plot_card {
        background: linear-gradient(rgba(255, 255, 255, 0.65), rgba(255, 255, 255, 0.65)), url('", logo_url, "');
        background-size: contain;
        background-position: center;
        background-repeat: no-repeat;
      }
      #nav_card, #nav_card .card-body {
        overflow: visible !important;
      }
      #nav_card .form-group {
        margin-bottom: 0 !important;
      }
      .selectize-dropdown {
        z-index: 9999 !important;
      }
    ")
    tags$style(HTML(css))
  })
  
  output$kicker_name <- renderText({ selected_data()$kicker_name_clean })
  output$kicker_id   <- renderText({ selected_data()$kicker_player_id })
  
  # FIXED: Backslashes removed from style string
  output$kicker_headshot <- renderUI({
    tags$img(src = selected_data()$headshot, 
             style = "width: 100%; max-width: 260px; border-radius: 50%; border: 3px solid #f8f9fa; box-shadow: 0 4px 10px rgba(0,0,0,0.1);")
  })
  
  output$spider_chart <- renderPlotly({
    k <- selected_data()
    plot_ly(
      type = 'scatterpolar',
      r = c(k$indoor_kcks, k$outdoor_kcks, k$bad_wthr_kcks, k$good_wthr_kcks, k$makes, k$exp_makes),
      theta = c('Indoors', 'Outdoors', 'Bad Weather', 'Good Weather', 'Actual Makes', 'Expected Makes'),
      fill = 'toself',
      fillcolor = 'rgba(44, 62, 80, 0.5)',
      line = list(color = '#2c3e50', width = 2),
      marker = list(color = '#2c3e50', size = 8),
      # FIXED: Backslashes removed from hovertemplate
      hovertemplate = "<b>%{theta}</b><br>Count: %{r}<extra></extra>"
    ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, max(c(k$attempts, k$exp_makes, 40))))
        ),
        showlegend = FALSE,
        paper_bgcolor = 'rgba(0,0,0,0)', 
        plot_bgcolor = 'rgba(0,0,0,0)',
        margin = list(t = 30, b = 30, l = 30, r = 30)
      )
  })
  
  output$leaderboard_table <- renderDT({
    display_data <- xfgoe_ldrbrd %>%
      mutate(Headshot = paste0('<img src="', headshot, '" height="40" style="border-radius: 50%;">')) %>%
      select(Headshot, Kicker = kicker_name_clean, Attempts = attempts, Makes = makes,
             `Expected` = exp_makes, `Total xFGOE` = total_xfgoe, `xFGOE/Kick` = xfgoe_per_kck, `FG%` = fgp) %>%
      arrange(desc(`Total xFGOE`))
    
    datatable(display_data, escape = FALSE, rownames = FALSE, filter = 'top',
              options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100), autoWidth = FALSE, scrollX = TRUE,
                             dom = '<"top"lf>rt<"bottom"ip><"clear">'),
              class = 'cell-border stripe hover') %>%
      formatRound(c("Expected", "Total xFGOE"), digits = 1) %>%
      formatRound("xFGOE/Kick", digits = 3) %>%
      formatPercentage("FG%", digits = 1)
  })
}

shinyApp(ui, server)