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
    get_PlayerCareerStats_by_name(input$player)
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

  output$playerScoring <- renderPlotly({
    plot_playerScoring(player_career_stats())
  })

})
