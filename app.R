#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(tidyverse)
library(httr)
library(jsonlite)
library(readr)
require(DBI)
require(dbplyr)
require(RSQLite)
require(stringr)
library(reactable)

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("Style/shlstylesheet.css"),
  # Application title
  titlePanel("SHL Hall of Fame Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "select",
        h3("Playoffs"),
        choices = list(
          "All" = 1,
          "Playoffs" = 2,
          "Regular Season" = 3
        ),
        selected = "All"
      ),
      width = 2
    ),

    # Show a plot of the generated distribution
    mainPanel(reactableOutput("skaterCareerStats"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  con <- dbConnect(SQLite(), "database/SHLHistory.db")
  dbConnect(SQLite(), "database/SHLHistory")

  options(
    reactable.theme = reactableTheme(
      color = "#FFFFFF",
      backgroundColor = "#262626",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "#2b2b2b",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)"),
      cellStyle = list(
        display = "flex",
        flexDirection = "column",
        justifyContent = "center"
      )
    )
  )

  output$skaterCareerStats <- renderReactable({

    test_table <- filter(skater_season_stats, !grepl('CPU', playerName))
    if(input$select != 1){
      if(input$select == 2){
        skater_season_stats <- filter(skater_season_stats, isPlayoffs == 1)
      }
      else{
        skater_season_stats <- filter(skater_season_stats, isPlayoffs == 0)
      }
    }
    reactable(
      test_table,
      #skater_season_stats[c("playerName", "teamName", "Season", "GamesPlayed", "Goals", "Assists", "Points", "PlusMinus", "PenaltyMinutes", "Hits", "Shots", "ShotsBlocked", "MinutesPlayed", "PPGoals", "PPAssists", "PPPoints", "PPMinutes", "PKGoals", "PKAssists", "PKPoints", "PKMinutes", "GameWinningGoals", "FaceoffsTotal", "FaceoffWins", "FightsWon", "FightsLost", "GvA", "TkA")],
      bordered = TRUE,
      filterable = TRUE,
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "112.9%",
      defaultColDef = colDef(align = "center", ),
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
