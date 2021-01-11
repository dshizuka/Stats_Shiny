#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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
# Define server logic required to draw a histogram
server <- function(input, output, session){
    values <- reactiveValues()
    
    data <- reactive({
        req(input$upload)
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ","),
               tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
               validate("Invalid file; Please upload a .csv or .tsv file")
        )
    })
    
    
    #this updates the drop-down menu for selecting variables to plot given the data by taking the column names of the dataframe.
    observeEvent(data(),{
        choices=names(data())
        updateSelectInput(session, "var1", choices=choices)
    })
    
    output$head <-   renderTable({
        head(data(), input$n)
    })
    
    #
    output$var.check <- renderPrint({
        sapply(as.data.frame(data()), class)
    })
    
    # # ##get data ready to plot
    # dataSelection <- reactive({
    #     dat <- data()
    #     chosen.data <- dat %>% select(input$chosen.value)
    #     return(chosen.data)
    # }) 
    # 
    
    plotit = reactive({
        require(ggplot2)
        dat<- as.data.frame(data())
        chosen.var=input$var1
        slim.dat=dat%>%select(input$var1)
        #histogram with one group
        if(input$plot.type==1){
            if(class(dat[,chosen.var])=="numeric"){
                values$p <-  ggplot(data = slim.dat, aes(slim.dat[[1]])) + geom_histogram(fill="gray", color="black") + theme_classic() + xlab(input$var1) + geom_vline(aes(xintercept = mean(slim.dat[[1]])),lty=2,size=1)
                values$p
            }
            else{
                plot(1:2, 1:2, xaxt="n", yaxt="n", type="n", bty="n", xlab="", ylab="")
                text(x=1.5, y=1.5, labels="The chosen data is not a numerical value", cex=2)
            }
        }
    })
    
    # 
    output$plot = renderPlot({
        print(plotit())
    })
    
    #
    output$export <- downloadHandler(
        filename <- function(){
            paste("plot_", strftime(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".pdf", sep = "")},
        content = function(file){
            pdf(file, onefile = T)
            print(values$p)
            dev.off()
        }
        #        contentType = 'image/pdf'
    )
}
