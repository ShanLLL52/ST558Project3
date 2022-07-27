library(shinydashboard)
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$img <- renderImage({
      list(src = "dataset-cover")
    } ,deleteFile = FALSE)
    
    output$text <- renderText({
      "The purpose of the app is to explore data using different plots and tables, and model the data using different supervised learning models."
    })
})