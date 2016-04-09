library(shiny)

shinyUI(fluidPage(theme="bootstrap.min.css",
  
  # Application Title
  titlePanel("NextWord: Predictive Text Model"),
  
   fluidRow(
     mainPanel(
       tags$form(class="form form-inline",
     textInput("text",label=h3("Enter some text")),
     actionButton("submitPhrase","Submit Phrase to Dictionary")),
     h4("Prediction:"),
     textOutput("result")
     )
   )  
  
))