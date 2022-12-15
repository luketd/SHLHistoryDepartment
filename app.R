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
source("Scripts/table_setup.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("Style/shlstylesheet.css"),
  # Application title
  titlePanel("SHL Hall of Fame Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    selectInput(
      "type",
      h3("Data"),
      choices = list("Career" = "Career",
                     "Franchise" = "Franchise",
                     "Season-by-season" = "Season"),
      selected = "Season-by-season"
    ),
    selectInput(
      "season",
      h3("Playoffs"),
      choices = list("Playoffs" = "Playoffs",
                     "Regular Season" = "RegSeason"),
      selected = "RegSeason"
    ),
    width = 2
  ),

  # Show a plot of the generated distribution
  mainPanel(tabsetPanel(
    tabPanel("Player Stats",
             reactableOutput("stats_table"))
  )))
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

  output$stats_table <- renderReactable({
    display_table <- return_table(input$type, input$season)
    reactable(
      display_table,
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
