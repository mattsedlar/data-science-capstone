library(shiny)

shinyUI(fixedPage(theme="bootstrap.min.css",
                  
                  tags$head(
                    tags$style(HTML("
@import url('//fonts.googleapis.com/css?family=Roboto:900');

body { 
background-color: #04080C;
color: lightblue;
}

h2, h3 {
font-family:Roboto,sans-serif;
}

#main { 
border: 1px solid #fff;
padding: 0 24px 24px 24px;
box-shadow: 5px 5px 15px #0B1726;
}

#result {
font-size: 18px;
}
                                  "))
                ),
  
  # Application Title
  titlePanel("NexWord: Predictive Text Generator"),
  
  column(10,id="main",
         tags$form(class="form form-inline", role="form",
                   textInput("text",label=h3("Enter some text"))),
         tags$br(),
         actionLink("submitPhrase","Submit Phrase to Dictionary"),
         h4("Predictions:"),
         htmlOutput("result",class="label label-success")
  )
  
))