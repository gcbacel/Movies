## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(dplyr)
library(stringr)

source('functions/helpers.R')
genrelist = c("Animation","Children's","Comedy","Adventure","Fantasy","Romance","Drama","Action","Crime",
              "Thriller","Horror","Sci-Fi","War","Musical","Documentary","Mystery","Film-Noir","Western") 

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(disable = TRUE),

          dashboardBody(includeCSS("css/movies.css"),

                        
              fluidRow(
                  box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      collapsed = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings')
                      )
                  )
                ),
              fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    title = "Step 2: Discover movies you might like based on your ratings (System II)",
                    helpText("More movies you rate in step 1, better are the results"),
                    withBusyIndicatorUI(
                      actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results")
                  )
               ),
              
              fluidRow(
                box(width = 12, title = "BONUS: Top 10 movies rated on MovieLens by genre (System I)", status = "info", solidHeader = TRUE, collapsible = TRUE,
                    collapsed = TRUE,
                    div(class = "suggestionitems"
                    ),
                    selectInput('genreinput', 'Select a genre to see top movies rated (1-5) by +6,000 people', genrelist),
                    br(),
                    helpText("List of best rated movies by genre"),
                    tableOutput("genresuggestions")
                )
              )
              

              
              
          )
    )
) 