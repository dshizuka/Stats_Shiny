#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
library(tidyverse)
library(ggplot2)
library(patchwork)

DF=iris

# Define UI for application that draws a histogram
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
                                          selectInput("chosen.value",  "Choose a value to plot", choices = names(DF), selected = 1),
                                          br(),
                                          downloadButton("export", label = "Download your figure")
                                      ),
                                      mainPanel(
                                          plotOutput("histogram")
                                      )
                                  )
                         )))

# Define server logic required to draw a histogram
server <- function(input, output, session){
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
        saveRDS(finalDF, file="table.rds")
    })
    
    ##get data ready to plot
    dataSelection <- reactive({
        dat = readRDS("table.rds")
        chosen.data <- dat %>% select(input$chosen.value)
    }) 
    
    # 
    output$histogram = renderPlot({
        require(ggplot2)
        chosen.data <- dataSelection()
        if(class(chosen.data[,1])=="numeric"){
            values$p <-  ggplot(data = chosen.data, aes(chosen.data[,1])) + geom_histogram(fill="gray", color="black") + theme_classic()
            values$p
        }
        else{
            plot(1:2, 1:2, xaxt="n", yaxt="n", type="n", bty="n", xlab="", ylab="")
            text(x=1.5, y=1.5, labels="The chosen data is not a numerical value", cex=2)
        }
    })
    
    #
    output$export <- downloadHandler(
        filename <- "trialplot.pdf",
        #        filename <- function(){
        #            paste("stats_graphs_", strftime(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".pdf", sep = "")},
        content <- function(file){
            pdf(file, "plot.pdf", onefile = T)
            grid.arrange(values$p)
            dev.off()
        }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
