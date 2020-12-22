#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)

DF=iris
outfilename="table.csv"
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
            values$p <- hist(chosen.data, breaks=20)}
        else{
            plot(1:2, 1:2, xaxt="n", yaxt="n", type="n", bty="n", xlab="", ylab="")
            text(x=1.5, y=1.5, labels="The chosen data is not a numerical value", cex=2)
        }
    })
    
    #
    output$export <- downloadHandler(
        filename = function(){paste("stats_graphs_", strftime(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".pdf", sep = "")},
        content = function(file){
            pdf(file, onefile = T, width = 8.5, height = 11)
            values$p
    dev.off()
        })
})

