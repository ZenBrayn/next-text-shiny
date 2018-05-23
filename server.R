
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

source("prediction_model.R")

# Read in the prediction model
print("Reading model data")
mdl <- readRDS("prediction_model.rds")
print("Finished reading model data")


shinyServer(function(input, output) {
  
  nextWords <- reactive({
    results <- predictNextWordWithBackoffTopN(mdl,input$text, input$n_slider, TRUE, TRUE)
  })
  
  output$text1 <- renderText({
    top_word <- names(nextWords()[[1]])[1]
    paste("Next word is:", top_word)
  })
  
  output$text2 <- renderText({
    top_word <- names(nextWords()[[1]])[1]
    paste(" \"", input$text, " ", top_word, "\"", sep="")
  })
  
  output$text3 <- renderText({
    mdl_n <- nextWords()[[2]]
    paste("This result was determined with a ", mdl_n, "-gram model.", sep="")
  })
  
  output$plot <- renderPlot({
    plotNextWordFreqs(nextWords()[[1]])
  })

})
