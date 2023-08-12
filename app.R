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
library(dplyr)
source("Scripts/table_setup.R")

# Define UI for application that draws a histogram
ui <-
  navbarPage(
    "SHL Career Data",
    header = includeCSS("Style/shlstylesheet.css"),
    tabPanel("Stats", fluidPage(
      # Application title
      titlePanel("SHL Career Data"),

      # Sidebar with a slider input for number of bins
      sidebarLayout(sidebarPanel(
        selectInput(
          "season",
          h4("Playoffs"),
          choices = list("Regular Season" = "RegSeason", "Playoffs" = "Playoffs"),
          selected = "RegSeason"
        ),
        width = 2
      ),

      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Season Stats",
                   reactableOutput("season_stats_table")),
          tabPanel("Career Stats",
                   reactableOutput("career_stats_table")),
          tabPanel("Franchise Stats",
                   reactableOutput("franchise_stats_table")),
          tabPanel("Goalie Season Stats",
                   reactableOutput("goalie_season_stats_table")),
          tabPanel("Goalie Career Stats",
                   reactableOutput("goalie_career_stats_table")),
          tabPanel("Goalie Franchise Stats",
                   reactableOutput("goalie_franchise_stats_table"))
        )
      ))
    )),
    tabPanel(
      "Leaderboards",
      titlePanel("SHL Hall of Fame Data"),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "era",
            h4("SHL Era"),
            choices = list(
              "Custom",
              "Dead Puck Era (S1-S5)",
              "Experimental Era (S6-S8)",
              "Inflation Era (S9-S11)",
              "52-Game Era (S21-S24)",
              "Realism Era (S12-S20)",
              "Realism Era (S25-S52)",
              "STHS Era (S1-S52)",
              "FHM6 Era",
              "66-Game FHM6 Era",
              "50-Game FHM6 Era",
              "FHM8 Records"
            ),
            selected = "Custom"
          ),
          selectInput(
            "timeline",
            h4("Timeline"),
            choices = list("Regular Season" = "RegSeason", "Playoffs"),
            selected = "RegSeason"
          ),
          selectInput(
            "statLeader",
            h4("Stat"),
            choices = list(
              'Goals',
              'Assists',
              'Points',
              'PlusMinus',
              'PenaltyMinutes',
              'Hits',
              'Shots',
              'ShotsBlocked',
              'MinutesPlayed',
              'PPGoals',
              'PPAssists',
              'PPPoints',
              'PPMinutes',
              'PKGoals',
              'PKAssists',
              'PKPoints',
              'PKMinutes',
              'GameWinningGoals',
              'FaceoffsTotal',
              'FaceoffWins',
              'FightsWon',
              'FightsLost',
              'GvA',
              'TkA'
            ),
            selected = "Goals"
          ),
          h4("Season Range"),
          uiOutput("eraSlider"),
          width = 2
        ),
        mainPanel(plotOutput("kdaBar"))
      )
    ),
    tabPanel(
      "Achievements",
      reactableOutput("achievementTable")
    ),
    tabPanel(
      "HoF Eligibility Tool",
      reactableOutput("hofEligibility"),
      align="center"
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  con <- dbConnect(SQLite(), "database/SHLHistory.db")

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

  output$season_stats_table <- renderReactable({
    display_table <- return_table("Season", input$season)
    reactable(
      display_table,
      bordered = TRUE,
      filterable = TRUE,
      columns = list(
        teamName = colDef(
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf(
                "Reactable.setFilter('team-select', '%s', event.target.value || undefined)",
                name
              ),
              tags$option(value = "", "All"),
              lapply(unique(values), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px; background-color: #262626;"
            )
          }
        ),
        Pos = colDef(
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf(
                "Reactable.setFilter('team-select', '%s', event.target.value || undefined)",
                name
              ),
              tags$option(value = "", "All"),
              lapply(unique(values), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px; background-color: #262626;"
            )
          }
        )
      ),
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "112.9%",
      defaultColDef = colDef(align = "center"),
      elementId = "team-select"
    )
  })

  output$career_stats_table <- renderReactable({
    display_table <- return_table("Career", input$season)
    reactable(
      display_table,
      bordered = TRUE,
      filterable = TRUE,
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "112.9%",
      defaultColDef = colDef(align = "center"),
    )
  })

  output$franchise_stats_table <- renderReactable({
    display_table <- return_table("Franchise", input$season)
    reactable(
      display_table,
      bordered = TRUE,
      filterable = TRUE,
      columns = list(teamName = colDef(
        filterInput = function(values, name) {
          tags$select(
            onchange = sprintf(
              "Reactable.setFilter('team-select', '%s', event.target.value || undefined)",
              name
            ),
            tags$option(value = "", "All"),
            lapply(unique(values), tags$option),
            "aria-label" = sprintf("Filter %s", name),
            style = "width: 100%; height: 28px; background-color: #262626;"
          )
        }
      )),
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "112.9%",
      defaultColDef = colDef(align = "center"),
      elementId = "team-select"
    )
  })

  output$goalie_season_stats_table <- renderReactable({
    display_table <- return_table("SeasonG", input$season)
    reactable(
      display_table,
      bordered = TRUE,
      filterable = TRUE,
      columns = list(
        teamName = colDef(
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf(
                "Reactable.setFilter('team-select', '%s', event.target.value || undefined)",
                name
              ),
              tags$option(value = "", "All"),
              lapply(unique(values), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px; background-color: #262626;"
            )
          }
        ),
        Pos = colDef(
          filterInput = function(values, name) {
            tags$select(
              onchange = sprintf(
                "Reactable.setFilter('team-select', '%s', event.target.value || undefined)",
                name
              ),
              tags$option(value = "", "All"),
              lapply(unique(values), tags$option),
              "aria-label" = sprintf("Filter %s", name),
              style = "width: 100%; height: 28px; background-color: #262626;"
            )
          }
        )
      ),
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "112.9%",
      defaultColDef = colDef(align = "center"),
      elementId = "team-select"
    )
  })

  output$goalie_career_stats_table <- renderReactable({
    display_table <- return_table("CareerG", input$season)
    reactable(
      display_table,
      bordered = TRUE,
      filterable = TRUE,
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "112.9%",
      defaultColDef = colDef(align = "center"),
    )
  })

  output$goalie_franchise_stats_table <- renderReactable({
    display_table <- return_table("FranchiseG", input$season)
    reactable(
      display_table,
      bordered = TRUE,
      filterable = TRUE,
      columns = list(teamName = colDef(
        filterInput = function(values, name) {
          tags$select(
            onchange = sprintf(
              "Reactable.setFilter('team-select', '%s', event.target.value || undefined)",
              name
            ),
            tags$option(value = "", "All"),
            lapply(unique(values), tags$option),
            "aria-label" = sprintf("Filter %s", name),
            style = "width: 100%; height: 28px; background-color: #262626;"
          )
        }
      )),
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "112.9%",
      defaultColDef = colDef(align = "center"),
      elementId = "team-select"
    )
  })

  output$kdaBar <- renderPlot({
    topChart <- return_table("Season", input$timeline) %>%
      filter((input$seasonSlider[[1]] <= Season) &
               (input$seasonSlider[[2]] >= Season)) %>%
      select(playerName, Season, Stat = input$statLeader) %>%
      group_by(playerName) %>%
      mutate(name = paste0(playerName, ' (S', Season, ')')) %>%
      summarise(name, Season, stat = Stat) %>%
      ungroup(playerName) %>%
      arrange(desc(stat)) %>%
      top_n(10) %>%
      unique()

    ggplot(topChart, aes(reorder(name, stat), y = stat)) +
      geom_bar(stat = "identity", fill = "#4cc9f0") +
      geom_text(
        aes(label = stat),
        size = 5,
        hjust = -0.5,
        color = "#FFFFFF"
      ) +
      coord_flip() +
      ggtitle(paste0("Top 15 seasons by ", input$statLeader)) +
      xlab("Player Name") +
      ylab(input$statLeader) +
      theme_minimal(base_size = 20) +
      theme(plot.background = element_rect(color = "#FFFFFF")) +
      theme(plot.title = element_text(hjust = 0.5, color = "#FFFFFF")) +
      theme(axis.text.x = element_text(colour = "#FFFFFF")) +
      theme(axis.text.y = element_text(colour = "#FFFFFF")) +
      theme(axis.title = element_text(colour = "#FFFFFF")) +
      theme(plot.background = element_rect(fill = "#262626")) +
      theme(panel.grid.major.y = element_blank()) +
      theme(panel.grid.major.x = element_blank())
  })

  output$eraSlider <- renderUI({
    sliderInput(
      "seasonSlider",
      "",
      min = ifelse(input$era!="Custom", return_era_seasons(input$era)[[1]], 1),
      max = ifelse(input$era!="Custom", return_era_seasons(input$era)[[2]], max(shl_rs_stats$Season)),
      value =  c(1, max(
        shl_rs_stats$Season
      )),
      step = 1,
      round = TRUE
    )
  })

  output$achievementTable <- renderReactable({
    display_table <- achievements_table
    reactable(
      display_table,
      bordered = TRUE,
      filterable = TRUE,
      columns = list(Achievement = colDef(
        align = "center",
        filterInput = function(values, name) {
          tags$select(
            onchange = sprintf(
              "Reactable.setFilter('team-select', '%s', event.target.value || undefined)",
              name
            ),
            tags$option(value = "", "All"),
            lapply(unique(values), tags$option),
            "aria-label" = sprintf("Filter %s", name),
            style = "width: 100%; height: 28px; background-color: #262626; text-align: center;"
          )
        }
      )),
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      defaultColDef = colDef(align = "center"),
      elementId = "team-select"
    )
  })

  output$hofEligibility <- renderReactable({
    display_table <- hof_eligibility
    reactable(
      display_table,
      bordered = TRUE,
      filterable = TRUE,
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "50%",
      defaultColDef = colDef(align = "center"),
    )
  })
}

shinyApp(ui = ui, server = server)
