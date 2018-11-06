#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word Prediction"),
  p("This app will predict the next word from a phrase."),
  
  # Create the main panel where the user will enter a phrase
    mainPanel(
            h3("Enter phrase:"),
            textInput(inputId = "phrase", label = NULL, value = "", placeholder = "Type phrase here"),
            br(),
            
            submitButton("Submit"),
            br(),
            
            h4("Top 3 next word predictions:"),
            verbatimTextOutput("prediction"),
            br(),
            
            # Think about creating a histogram of top 10 predicted words based on probabilities
            plotOutput(outputId = "plot")
    )
  )
)
