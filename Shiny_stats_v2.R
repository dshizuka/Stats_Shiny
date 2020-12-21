library(shiny)
library(tidyverse)

#ui <- fluidPage(
#      fileInput("file", h3("File Input")),
#    hr(),
#    fluidRow(column(width=8,verbatimTextOutput("value"))),
#   )
#default_data=read.csv("domdat.2010.csv")
DF=iris
outfilename="table.csv"

ui <- shinyUI(navbarPage("Walking Through Stats",
                         tabPanel("Enter the data",
                                  sidebarLayout(
                                    sidebarPanel(
                                      helpText("Shiny app based on an example given in the rhandsontable package.", 
                                               "Right-click on the table to delete/insert rows.", 
                                               "Double-click on a cell to edit"),
                                      
                                      
                                      br(), 
                                      
                                      wellPanel(
                                        h3("Save"), 
                                        actionButton("save", "Save table")
                                      )        
                                      
                                    ),
                                    
                                    mainPanel(
                                      
                                      rHandsontableOutput("hot")
                                      
                                    )
                                  )
                                  ),
                         tabPanel("Plot the data",
                                  sidebarLayout( # give us a nice data selection side bar!
                                    sidebarPanel(
                                      h2("Try a histogram out for size"), # Some nice welcoming text for users
                                      br(), br(),
                                      selectInput("chosen.value",  "Choose a value to plot", choices = names(DF), selected = 1)
                                      ),
                                  mainPanel(
                                  plotOutput("histogram")
                                  )
                                  )
                         )))

server <- shinyServer(function(input, output, session){
  values <- reactiveValues()
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
#      rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")
#  })
  rhandsontable(DF,  stretchH = "all")
})
  
  ## Save 
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    saveRDS(finalDF, file=file.path(outdir, "table.rds"))
  })
  
##get data ready to plot
  dataSelection <- reactive({
    dat = readRDS("table.rds")
    chosen.data <- dat[,input$chosen.value] # The user's selection from the drop-down
  }) 
  
  # 
  output$histogram = renderPlot({
    chosen.data <- dataSelection()
    if(class(chosen.data)=="numeric"){
    hist(chosen.data, breaks=20)}
    else{
      plot(1:2, 1:2, xaxt="n", yaxt="n", type="n", bty="n", xlab="", ylab="")
      text(x=1.5, y=1.5, labels="The chosen data is not a numerical value", cex=2)
    }
  })
})


shinyApp(ui = ui, server = server)
