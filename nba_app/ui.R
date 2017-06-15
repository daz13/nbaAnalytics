#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

devtools::load_all(".")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("NBA Data Analytics"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 2,
      uiOutput("team"),
      uiOutput("player")
    ),

    # Show a plot of the generated distribution
    mainPanel( width= 10,
      tabsetPanel(
        tabPanel("Single Statistics",
                 uiOutput("stat_cat"),
                 fluidRow(plotlyOutput("playerPlot"))
        ),
        tabPanel("Scoring",
                 h3("Points"),
                 plotlyOutput("playerPoints"),
                 h3("Shooting volumes and Percentages"),
                 column(6,plotlyOutput("playerShootingVolumes")),
                 column(6,plotlyOutput("playerShootingPercentages"))
        )
      )
    )
  )
))
