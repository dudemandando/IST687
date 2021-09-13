library(shiny)
library(shinydashboard)
library(mapdeck)


countryMedals <- read.csv("C:\\Users\\danbu\\Documents\\GitHub\\IST687\\Dan\\OlympicGamesMap\\OlyMap\\medalSums.csv")
countryMedals$Gold <- countryMedals$Gold^2.5

ui <- dashboardPage(
    dashboardHeader()
    , dashboardSidebar(
        actionButton(inputId = "roads", label = "roads")
    )
    , dashboardBody(
        mapdeckOutput(outputId = "map")
    )
)

server <- function(input, output) {
    key <- "pk.eyJ1IjoiZHVkZW1hbmRhbmRvIiwiYSI6ImNqd2xqZDhkMzAwM2MzeXBmNWhjejViejUifQ.-qW3K4prGMIY0w7bP3LxUg"
    set_token(key)
    
    ## initialise a map
    output$map <- renderMapdeck({
        ######
        
        df <- capitals
        df$elev <- sample(50000:500000, size = nrow(df), replace = TRUE)
        
        mapdeck(style = mapdeck_style("dark"), pitch = 45) %>%
            add_column(
                data = countryMedals
                , lat = "lat"
                , lon = "long"
                , elevation = "Gold"
                , fill_colour = "Total.Athletes"
                , disk_resolution = 20
                , radius = 100000
                , tooltip = "team"
            )
        
        
        
        
        
        
        
        
        
        ######
        #mapdeck(token = key)
    })
    
}

shinyApp(ui, server)