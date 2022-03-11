

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
                                          h4("Try a histogram out for size"), 
                                          selectInput("chosen.value",  "Choose a value to plot", choices = NULL),
                                          br(),
                                          downloadButton("export", label = "Download a report")
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
    
    #this updates the drop-down menu for selecting variables to plot given the data by taking the column names of the dataframe.
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
    
      
    plotit = reactive({
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
    output$histogram = renderPlot({
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
