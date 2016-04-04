source("model.R")

shinyServer(function(input,output){
  
  output$result <- renderPrint(
    bea(input$text)
  )

})