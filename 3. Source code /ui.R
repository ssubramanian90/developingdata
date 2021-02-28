## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movies Recommender"),
          
          #dashboardSidebar(disable = TRUE),
          dashboardSidebar(
            sidebarMenu(
              menuItem("By Genre", tabName="GEN"),
              menuItem("By Rating", tabName="RAT")
            )
          ),

          dashboardBody(includeCSS("css/movies.css"),
                        tabItems(
                          tabItem(tabName = "RAT",
                                  fluidRow(
                                    box(width = 12, title = "Step 1: Rate as many movies as possible", 
                                        status = "info", solidHeader = TRUE, collapsible = TRUE,
                                        div(class = "rateitems",
                                            uiOutput('ratingsByRating')
                                        )
                                    )
                                  ),
                                  fluidRow(
                                    useShinyjs(),
                                    box(
                                      width = 12, status = "info", solidHeader = TRUE,
                                      title = "Step 2: Discover movies you might like",
                                      br(),
                                      withBusyIndicatorUI(
                                        actionButton("btn1", "Click here to get your recommended movies", class = "btn-warning")
                                      ),
                                      br(),
                                      tableOutput("resultsByRating")
                                    )
                                  )

                          ),
                          
                          tabItem(tabName = "GEN",
                                  fluidRow(
                                    
                                    column(3,
                                           selectInput("select", h3("Select genre"), 
                                                       choices = 
                                                         c("Action", "Adventure", "Animation", 
                                                                   "Children's", "Comedy", "Crime",
                                                                   "Documentary", "Drama", "Fantasy",
                                                                   "Film-Noir", "Horror", "Musical", 
                                                                   "Mystery", "Romance", "Sci-Fi", 
                                                                   "Thriller", "War", "Western"), 
                                                       selected = "Action")
                                           #verbatimTextOutput("selected_genre_index")
                                        
                                           )
                                  ),
                                  fluidRow(
                                    useShinyjs(),
                                    box(
                                      width = 12, status = "info", solidHeader = TRUE,
                                      title = "Step 2: Discover movies you might like",
                                      br(),
                                      withBusyIndicatorUI(
                                        actionButton("btn2", "Click here to get your recommended movies", 
                                                     class = "btn-warning")
                                      ),
                                      br(),
                                      tableOutput("resultsByGenre")
                                    )
                                  )
                          )
                        )
          )
    )
) 