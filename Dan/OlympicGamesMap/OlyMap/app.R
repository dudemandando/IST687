library(shiny)
library(leaflet)

#install.packages("leaflet.minicharts")

countryMedals <- read.csv("C:\\Users\\danbu\\Documents\\GitHub\\IST687\\Dan\\OlympicGamesMap\\OlyMap\\medalSums.csv")
countryMedals$Gold <- countryMedals$Gold^10
key <- "pk.eyJ1IjoiZHVkZW1hbmRhbmRvIiwiYSI6ImNqd2xqZDhkMzAwM2MzeXBmNWhjejViejUifQ.-qW3K4prGMIY0w7bP3LxUg"
colors <- c("#4c34eb", "#eb9934", "#eb3434")
MiniBarDf <- data.frame(countryMedals$lat, countryMedals$long, countryMedals$Gold, countryMedals$Silver, countryMedals$Bronze)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage(
    
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Maps", 
                tabName = "maps", 
                icon = icon("globe"),
                menuSubItem("Countries", tabName = "m_country", icon = icon("map")),
                menuSubItem("Total Athletes", tabName = "m_tot_ath", icon = icon("map"))
            ),
            menuItem(
                "Charts", 
                tabName = "charts", 
                icon = icon("bar-chart"),
                menuSubItem("Watersheds", tabName = "c_water", icon = icon("area-chart")),
                menuSubItem("Population", tabName = "c_pop", icon = icon("area-chart"))
            )
        )
    ),
    dashboardBody(
        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
        leafletOutput("map")
    )
)

server <- function(input, output, session) {
    
 
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(group="OSM")%>%
            setView(lat = 43.191558,
                    lng = 16.089341,
                    zoom = 4
            ) %>% 
            addCircles(lng=countryMedals$long, 
                       lat=countryMedals$lat,
                       radius=countryMedals$Total.Athletes*50, 
                       popup=paste("Total Athletes", as.character(countryMedals$Total.Athletes, " ")),
                       color = countryMedals$Total.Athletes,
                       stroke=FALSE
            )%>%
            addMarkers(lng=countryMedals$long, 
                       lat=countryMedals$lat,
                       popup=paste(countryMedals$team),
                    
            )
                        
        
        
            
    })
}



shinyApp(ui, server)