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
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Supreme Court Decisions 1946-2016"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("daterange", 
                      label=h3("Date Range"), 
                      min= 1946, 
                      max=2016, 
                      value=c(1950,1980),
                      sep="")
    ),
    # Show a plot of the generated distribution
    mainPanel(
       #plotOutput("distPlot"),
       plotOutput("voteplot"),
       plotOutput("decisionplot"),
       plotOutput("decisionsplitplot"),
       plotOutput("precedentplot")
    )
  )
))
