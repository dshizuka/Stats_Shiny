

library(shiny)
library(rhandsontable)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(survival)


# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Walking Through Stats",
                         tabPanel("Enter the data",
                                  sidebarLayout(
                                      sidebarPanel(
                                          wellPanel(
                                              h3("Upload data file (.csv format)"),
                                              fileInput("upload", NULL)
                                              ),
                                          br(), 
                                          numericInput("n", "# rows to show", value = 5, min = 1, step = 1),
                                          br(),
                                      ),
                                      mainPanel(
                                          "Once you upload your data on the left, the first several rows of the data will show up as a table. Check to make sure that you've uploaded the correct table and that the variables are categorized correctly. Once it looks good, move on to the 'Plot the data' section",
                                          tableOutput("head"),
                                          
                                          h3(verbatimTextOutput("var.check"))
                                      )
                         )
                         ),
                         tabPanel("Plot the data",
                                  sidebarLayout( # give us a nice data selection side bar!
                                      sidebarPanel(
                                          h4("Plot a Kaplan-Meier Curve"), 
                                          selectInput("latency",  "Choose a latency value to plot", choices = NULL),
                                          selectInput("event",  "Choose the event column", choices = NULL),
                                          selectInput("Sex",  "Choose the grouping (i.e., treatment) variable", choices = NULL),
                                          br(),
                                          downloadButton("export", label = "Download a report")
                                          ),
                                      mainPanel(
                                          plotOutput("KM")
                                  )
                         ))
                         ))

# Define server logic required to draw a K-M plot
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
        updateSelectInput(session, "latency", choices=choices)
        updateSelectInput(session, "event", choices=choices)
        updateSelectInput(session, "Sex", choices=choices)
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
          chosen.data <- dat %>% select(input$latency, input$event, input$Sex)
          return(chosen.data)
      }) 
    
      sf <- reactive({
        chosen.data <-chosen.data()
        sf <- survfit(Surv(latency, event)~Sex, data=chosen.data)
        return(sf)
      })
      
    plotit = reactive({
        require(ggplot2)
      require(survival)
        chosen.data <- dataSelection()
        sf <- sf
            values$p <-  plot(sf, col=c("red", "blue"), las=1, xlab = "time(minutes)", ylab = "proportion not learned")
            values$p
        })
    
    # 
    output$KM = renderPlot({
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

# Run the application 
shinyApp(ui = ui, server = server)
