
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("NextText: A Text Prediction Application"),

  h4("Enter a text phrase and this app will predict the next word!"),
  
  h5("Instructions"),
  
  tags$li("Enter a text phrase into the text field below"),
  tags$li("Click the Submit button and wait for the results (it may take a few seconds)"),
  tags$li("OPTIONAL: Select the number of top words to display via the slider to show more than just the top hit"),
  
  h5("Notes"),
  
  tags$li("Please wait until the default output is displayed on the right (5-10 seconds) when first visiting the web page.  The application needs time to start up."),
  tags$li("This application uses an N-gram model to predict the next word given the previous N-1 words."),
  tags$li("The app will first try a 4-gram model, and default to lower order models if the specific predictions are not available."),
  tags$li("The output displays the order of the model used.  Be wary of results from 1-gram models.  None of the prior words are used here for the prediction."),
  
  hr(),
  
  # 
  sidebarLayout(
    sidebarPanel(
      textInput("text",
                strong("Enter text for next word prediction"),
                "Let's go to the"),
      
      br(),
      
      sliderInput("n_slider",
                  strong("Number of Top Words to Display"),
                  1,
                  10,
                  5,
                  1),
      
      br(),
      
      submitButton("Submit")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h4(textOutput("text1")),
      textOutput("text2"),
      textOutput("text3"),
      plotOutput("plot", width = 400, height = 300)
    )
  )
))
