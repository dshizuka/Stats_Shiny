#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

DF=iris

shinyUI(navbarPage("Walking Through Stats",
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
                                          downloadButton('export', label = "Download your figure")
                                      ),
                                      mainPanel(
                                          plotOutput("histogram")
                                      )
                                  )
                         )))
