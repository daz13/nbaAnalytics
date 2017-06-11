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

  output$playerPlot <- renderPlot({
    dat <- get_PlayerCareerStats_by_name(input$player)
    plot_PlayerStats(dat, "PTS")
  })

})
