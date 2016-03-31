library(shiny)

shinyUI(fluidPage(
  # Application Title
  titlePanel("Predictive Text Model"),
  
   fluidRow(
     textInput("text",label=h3("Text Input"))
   )  
  
))