source("model.R")

shinyServer(function(input,output,session){
  

  output$result <- renderPrint(
    betty(input$text)
  )
  
  observeEvent(input$submitPhrase,{
    output$dialog <- renderUI({
      phraseinator(input$text)
    })
    
  })

})