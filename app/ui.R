library(shiny)

shinyUI(fluidPage(
  # Application Title
  titlePanel("Predictive Text Model"),
  
   fluidRow(
     mainPanel(
     textInput("text",label=h3("Text Input")),
     textOutput("result")
     )
   )  
  
))