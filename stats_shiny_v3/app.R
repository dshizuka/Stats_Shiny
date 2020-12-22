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


# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Walking Through Stats",
                         tabPanel("Enter the data",
                                  sidebarLayout(
                                      sidebarPanel(
                                          wellPanel(
                                              h4("Upload data file (up to 5MB)"),
                                              fileInput("upload", NULL)
                                              ),
                                          br(), 
                                          numericInput("n", "# rows to show", value = 5, min = 1, step = 1),
                                          br(),
                                      ),
                                      mainPanel(
                                          "Upload your data on the left and check to make sure the variables are categorized correctly.",
                                          tableOutput("head"),
                                          
                                          h3(verbatimTextOutput("var.check"))
                                      )
                         )
                         ),
                         tabPanel("Plot the data",
                                  sidebarLayout( # give us a nice data selection side bar!
                                      sidebarPanel(
                                          h2("Try a histogram out for size"), # Some nice welcoming text for users
                                          br(), br(),
                                          selectInput("chosen.value",  "Choose a value to plot", choices = NULL),
                                          br(),
                                          downloadButton("export", label = "Download your figure")
                                          ),
                                      mainPanel(
                                          plotOutput("histogram")
                                  )
                         ))
                         ))

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
    
    observeEvent(data(),{
        choices=names(data())
        updateSelectInput(session, "chosen.value", choices=choices)
    })
    
    output$head <-   renderTable({
        head(data(), input$n)
    })

    #
    output$var.check <- renderPrint({
        sapply(as.data.frame(data()), class)
    })
    
    # ##get data ready to plot
      dataSelection <- reactive({
          dat <- data()
          chosen.data <- dat %>% select(input$chosen.value)
          return(chosen.data)
      }) 
    
    # 
    output$histogram = renderPlot({
        require(ggplot2)
        chosen.data <- dataSelection()
        if(class(chosen.data[[1]])=="numeric"){
            values$p <-  ggplot(data = chosen.data, aes(chosen.data[[1]])) + geom_histogram(fill="gray", color="black") + theme_classic()
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
