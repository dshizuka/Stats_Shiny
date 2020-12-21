library(shiny)
library(tidyverse)

ui <- fluidPage(
      fileInput("file", h3("File Input")),
    hr(),
    fluidRow(column(width=8,verbatimTextOutput("value"))),
   )



server <- shinyServer(function(input, output, session){
  output$value <- renderPrint({
    names(input$file)
  })
  })

shinyApp(ui = ui, server = server)
