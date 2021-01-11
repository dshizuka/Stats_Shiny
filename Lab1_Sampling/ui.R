#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


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
                                          h4("What kind of plot do you want to make?"), 
                                          selectInput("plot.type", label=NULL,  choices=c("CHOOSE PLOT TYPE" =0, "histogram (one group)" =1, "histogram (two groups)" =2, "boxplot" =3, "scatterplot" =4)),
                                          
                                          br(),
                                          selectInput("var1",  "Choose x variable", choices = NULL),
                                          br(),
                                          downloadButton("export", label = "Download a report")
                                      ),
                                      mainPanel(
                                          plotOutput("plot")
                                      )
                                  ))
))
