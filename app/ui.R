library(shiny)

shinyUI(fixedPage(theme="css/bootstrap.min.css",
                  
                  tags$head(
                    tags$style(HTML("
@import url('//fonts.googleapis.com/css?family=Roboto:900');

body { 
background-color: #04080C;
color: lightblue;
text-align:center;
margin: 0 auto;
}

h2, h3 {
font-family:Roboto,sans-serif;
}

#main { 
border: 1px solid #fff;
padding: 0 24px 24px 24px;
}

#result span {
font-size: 18px;
margin-right: 12px;
}
                                  "))

                ),
                includeCSS("www/css/jquery-ui.min.css"),
                includeScript("www/js/jquery-ui.min.js"),
                includeScript("www/js/script.js"),
  
  # Application Title
  titlePanel("NexWord: Predictive Text Generator"),
  
  column(12,id="main",
         tags$form(class="form form-inline", role="form",
                   textInput("text",label=h3("Enter some text"))),
         tags$br(),
         actionLink("submitPhrase","Submit Phrase to Dictionary"),
         h4("Predictions:"),
         htmlOutput("result"),
         uiOutput("dialog")
  )
  
))
