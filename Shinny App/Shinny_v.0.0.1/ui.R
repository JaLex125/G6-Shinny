

library(shinythemes)
library(igraph)
library(shiny)
library(data.table)
library(rsconnect)
library(ggplot2)
library(dplyr)





ui <- navbarPage(
  theme = shinytheme("lumen"),
  titlePanel("Spotify Songs"),
                 tabPanel("Based on Genre",
                          sidebarPanel(
                              selectInput(inputId = "genre", label = "genre",
                              unique(df.search.criteria$top_genre), multiple = FALSE),
                              verbatimTextOutput("rock"),

                          sliderInput(
                            inputId = "range", label = "Range of bpm that you wish to read?",
                            min = min(df.search.criteria$bpm),max = 100,value = c(55,100)
                            ),
                          ),
                          
                          mainPanel(
                            h2("Top songs by Genre"),
                          DT::dataTableOutput(outputId = "songsreco")
                          )
                  ),
  
  
              tabPanel("Based on Year",
                        sidebarPanel(
                            selectInput(inputId = "year", label = "Which year do you like?",
                            sort(unique(df.search.criteria$year), decreasing  = TRUE), multiple = FALSE),
                            verbatimTextOutput("2000"),
                      
                        sliderInput(inputId = "range_2", label = "Ragne of Ratings that you wish to read?",
                            min = min(df.search.criteria$popularity),max = 100,value = c(55,100))),

                         mainPanel(
                            h2("Top songs of the Year"),
                          DT::dataTableOutput(outputId = "songsreco_genre"))),

  
  
            tabPanel("Distribution of Song Duration",
                      sidebarLayout(
                          sidebarPanel(

                          sliderInput(inputId = "bins",
                          label = "Number of bins:",
                          min = 1,
                          max = 50,
                          value = 30)

                          ),

                      mainPanel(
                           plotOutput(outputId = "distPlot")

                              )
                          )
                    )
  
  )

ui



