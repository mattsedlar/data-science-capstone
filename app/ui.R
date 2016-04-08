library(shiny)

shinyUI(fluidPage(
  # Application Title
  titlePanel("NextWord: Predictive Text Model"),
  
   fluidRow(
     mainPanel(
     textInput("text",label=h3("Enter some text")),
     h4("Prediction:"),
     textOutput("result")

     )
   )  
  
))