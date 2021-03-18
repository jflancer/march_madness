library(shiny)
library(reactable)
library(dplyr)

team_rating <- read.csv("data/team_ratings.csv", stringsAsFactors = F) %>%
  mutate(across(matches("RAPM"), round, 1))
table_preds <- read.csv("data/table_predictions.csv", stringsAsFactors = F)
game_preds <- read.csv("data/game_predictions.csv", stringsAsFactors = F)
regions = c("East","Midwest","South", "West")

pred_pal <- function(x) rgb(colorRamp(c("#e6e9f6", "#4661d6"), bias = 1, alpha = T)(x), maxColorValue = 255)
rating_pal <- function(x) rgb(colorRamp(c("#e6e9f6", "#4661d6"), bias = 1, interpolate = "spline", alpha = T)(x), maxColorValue = 255)

pred_table <- function(df) {
  df %>%
  mutate(Seed = as.character(Seed), Region = as.factor(Region)) %>%
  select(Region, Seed, Team, "First Four" = "First4", "Round 1" = "Round1", "Round 2" = "Round2", "Sweet 16"= "Sweet16", "Elite 8"=  "Elite8", "Final Four" = "Final4", "Championship") %>%
  reactable(
    pagination = F,
    compact = T,
    defaultSorted = "Championship",
    defaultSortOrder = "desc",
    striped = F,
    bordered = T,
    defaultColDef = colDef(
      style = function(value) {
        if (!is.numeric(value)) return()
        color <- pred_pal(value/100)
        list(background = color)
      },
      minWidth = 50
    ),
    columns = list(
      "Region" = colDef(filterable = T),
      "Seed" = colDef(filterable = T),
      "Team" = colDef(filterable = T),
      "First Four" = colDef(align="center", format = colFormat(digits = 1)),
      "Round 1" = colDef(align="center", format = colFormat(digits = 1)),
      "Round 2" = colDef(align="center", format = colFormat(digits = 1)),
      "Sweet 16" = colDef(align="center", format = colFormat(digits = 1)),
      "Elite 8" = colDef(align="center", format = colFormat(digits = 1)),
      "Final Four" = colDef(align="center", format = colFormat(digits = 1)),
      "Championship" = colDef(align="center", format = colFormat(digits = 1))
    )
  )
}

rating_table <-
  team_rating %>%
  reactable(
    defaultPageSize = 25,
    width = "500px",
    height = "1000px",
    compact = T,
    pagination = F,
    defaultSorted = "RAPM",
    defaultSortOrder = "desc",
    striped = F,
    bordered = T,
    columns = list(
      Team = colDef(
        minWidth = 200
      ),
     ORAPM = colDef(
       name = "Offensive Rating",
       align = "center",
       style = function(value) {
         normalized <- (value - min(team_rating$ORAPM)) / (max(team_rating$ORAPM) - min(team_rating$ORAPM))
         color <- rating_pal(normalized)
         list(background = color)
         }
       ),
     DRAPM = colDef(
       name = "Defensive Rating",
       align = "center",
       style = function(value) {
         normalized <- (value - min(team_rating$DRAPM)) / (max(team_rating$DRAPM) - min(team_rating$DRAPM))
         color <- rating_pal(normalized)
         list(background = color)
       }
     ),
     RAPM = colDef(
       name = "Net Rating",
       align = "center",
       style = function(value) {
         normalized <- (value - min(team_rating$RAPM)) / (max(team_rating$RAPM) - min(team_rating$RAPM))
         color <- rating_pal(normalized)
         list(background = color)
       }
     )
    )
  )

march_madness_teams <- c(
  "Gonzaga",
  "Norfolk St.",
  "Appalachian St.",
  "Oklahoma",
  "Missouri",
  "Creighton",
  "UC Santa Barbara",
  "Virginia",
  "Ohio",
  "Southern California",
  "Wichita St.",
  "Drake",
  "Kansas",
  "Eastern Wash.",
  "Oregon",
  "VCU",
  "Iowa",
  "Grand Canyon",
  "Michigan",
  "Mt. St. Mary's",
  "Texas Southern",
  "LSU",
  "St. Bonaventure",
  "Colorado",
  "Georgetown",
  "Florida St.",
  "UNC Greensboro",
  "BYU",
  "Michigan St.",
  "UCLA",
  "Texas",
  "Abilene Christian",
  "UConn",
  "Maryland",
  "Alabama",
  "Iona",
  "Baylor",
  "Hartford",
  "North Carolina",
  "Wisconsin",
  "Villanova",
  "Winthrop",
  "Purdue",
  "North Texas",
  "Texas Tech",
  "Utah St.",
  "Arkansas",
  "Colgate",
  "Florida",
  "Virginia Tech",
  "Ohio St.",
  "Oral Roberts",
  "Illinois",
  "Drexel",
  "Loyola Chicago",
  "Georgia Tech",
  "Tennessee",
  "Oregon St.",
  "Oklahoma St.",
  "Liberty",
  "San Diego St.",
  "Syracuse",
  "West Virginia",
  "Morehead St.",
  "Clemson",
  "Rutgers",
  "Houston",
  "Cleveland St."
)

server <- function(input, output) {
  output$prediction_table <-
    renderReactable(
      expr = pred_table(filter(table_preds, Region %in% input$region))
    )
  output$ratings_table <-
    renderReactable(
      expr = rating_table
    )
  
  output$game_prediction <-
    renderText({
      game_prediction <- game_preds[which(game_preds$team == input$team1 & game_preds$opponent == input$team2),]
      prob <- pnorm(game_prediction$team_score - game_prediction$opp_score, 0, 11.7)
      paste(
        "\nExpected Score:", round(game_prediction$team_score), "-", round(game_prediction$opp_score), 
        "\nExpected Total:", round(game_prediction$team_score + game_prediction$opp_score),
        "\nExpected Differential:", round(game_prediction$team_score - game_prediction$opp_score,1), 
        "\n Expected Probability:", round(prob*100,2))
    })
}

ui <- fluidPage(
  navbarPage(
    "March Madness",
    tabPanel("Prediction Table",
             column(4, selectInput("region", "Region", choices = regions, selected=regions, multiple = T)),
             column(12, reactableOutput("prediction_table"), align = "center")
             ),
    tabPanel("Team Ratings",
             column(12, reactableOutput("ratings_table"), align = "center")
             ),
    tabPanel("Game Prediction",
             fluidRow(
               selectInput("team1", "Team 1", choices = sort(march_madness_teams)),
               selectInput("team2", "Team 2", choices = sort(march_madness_teams))
             ),
             column(6, verbatimTextOutput("game_prediction"))
             )
    )
  )

shinyApp(ui = ui, server = server)