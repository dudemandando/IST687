#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shinydashboard")
#install.packages("shinydashboardPlus")
#install.packages("shiny")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("magrittr")
#install.packages("highcharter")
#install.packages("shinyjs")
#install.packages("tmap")
#install.packages("tmaptools")
#install.packages("leaflet")
#install.packages("mapdeck")

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("IST 687 - Olympic Athlete Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}


# Run the application 
shinyApp(ui = ui, server = server)



#MapBox APi Token 

key <- 'pk.eyJ1IjoiZHVkZW1hbmRhbmRvIiwiYSI6ImNqd2xqZDhkMzAwM2MzeXBmNWhjejViejUifQ.-qW3K4prGMIY0w7bP3LxUg'
mapdeck(token = key)
mapdeck(token = key, style = 'mapbox://styles/mapbox/dark-v9')

