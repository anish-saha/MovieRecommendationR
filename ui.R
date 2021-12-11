library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(dplyr)
library(recommenderlab)
library(Matrix)


gc(verbose = FALSE)
source('functions/helpers.R')

shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movie Recommender"),
    
    #dashboardSidebar(disable = FALSE, collapsed = FALSE),
    dashboardSidebar(
      sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("Genre Recommendation",tabName = "genre"),
        menuItem("User Ratings Recommendation",tabName = "user")
        )
      ),
    dashboardBody(includeCSS("css/movie.css"),
                  tabItems
                  (
                    tabItem
                    (
                      tabName = "user",
                      fluidRow
                      (
                        box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                          div(class = "rateitems",
                              uiOutput('ratings')
                          )
                        )
                      ),
                      fluidRow
                      (
                        useShinyjs(),
                        box
                        (
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Discover movies you might like",
                          br(),
                          withBusyIndicatorUI
                          (
                            actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("results")
                        )
                      )
                  ),
                  tabItem
                    (
                      tabName = "genre",
                      fluidPage(
                        selectInput("genre", "Choose a genre:",
                                    list("Action", "Adventure", "Animation", 
                                         "Children's", "Comedy", "Crime",
                                         "Documentary", "Drama", "Fantasy",
                                         "Film-Noir", "Horror", "Musical", 
                                         "Mystery", "Romance", "Sci-Fi", 
                                         "Thriller", "War", "Western")
                        ),
                        br(),
                        tableOutput("result")
                      )
                    )
                )
    
    )
  )
) 