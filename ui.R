library(shiny)

shinyUI(fluidPage(
  titlePanel("2010-2018 WPD "),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", label="Year", 
                  min=2010,max=2018,value=2010,sep="",animate=animationOptions(interval=500,loop=TRUE)),      
      radioButtons("data",label="Data:",c("Total Arrests","White Only Arrests","Black Only Arrests")),
      radioButtons("adj",label="Data Adjustment:",c("None","As a Percent of the Population","As a Percent of Total Arrests",
                                                    "Standardized Incidence Ratio","Poisson Regression")),
    ),
    mainPanel(
      textOutput("text"),
      plotOutput("map"),
      tableOutput("table"))
  )))
