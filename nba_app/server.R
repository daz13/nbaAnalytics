#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(stringr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$team <- renderUI(
    selectInput("team",
                label = "Select team",
                choices = teams_list$TEAM_NAME)
  )

  players <- reactive({
    players_per_team[[stringr::str_to_lower(input$team)]]
  })

  output$player <- renderUI(
    selectInput("player",
                label = "Select player",
                choices = players()$DISPLAY_FIRST_LAST)
  )

  player_career_stats <- reactive({
    get_PlayerCareerStats_by_name(input$player, permode = input$permode)
  })

  output$stat_cat <- renderUI(
    selectInput("stat_cat",
                "Select Category",
                choices = names(player_career_stats()), selected = "PTS")
  )

  output$playerPlot <- renderPlotly({
    plot_PlayerStats(player_career_stats(),#,"PTS")
                     input$stat_cat)
  })

  output$playerPoints <- renderPlotly({
    plot_playerPoints(player_career_stats())
  })

  output$playerShootingVolumes <- renderPlotly({
    plot_playerStats(player_career_stats(), FGA, FG3A, FTA)
  })
  output$ playerShootingPercentages <- renderPlotly({
    plot_playerStats(player_career_stats(), FG_PCT, FG3_PCT, FT_PCT)
  })
  output$playerRebounds <- renderPlotly({
    plot_playerStats(player_career_stats(), REB, OREB, DREB)
  })
  output$playerAssists <-renderPlotly({
    plot_playerStats(player_career_stats(), AST, TOV)
  })
  output$playerDefense <-renderPlotly({
    plot_playerStats(player_career_stats(), STL, BLK, DREB)
  })

})
