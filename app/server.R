source("model.R")

shinyServer(function(input,output,session){
  

  output$result <- renderPrint(
    betty(input$text)
  )

})