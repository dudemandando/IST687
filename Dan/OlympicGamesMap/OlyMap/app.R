library(shiny)
library(leaflet)
library(shinydashboard)
library(scales)
library(leaflet.minicharts)

#install.packages("leaflet.minicharts")
#https://rstudio.github.io/leaflet/markers.html#circle-markers
countryMedals <- read.csv("C:\\Users\\danbu\\Documents\\GitHub\\IST687\\Dan\\OlympicGamesMap\\OlyMap\\medalSums.csv")



colors <- c("#FFFF33", "#CECCB7", "#EE9A25")
MiniBarDf <- data.frame(countryMedals$Gold, countryMedals$Silver, countryMedals$Bronze)
colnames(MiniBarDf) <- c("Gold", "Silver", "Bronze")
MiniBarDf <- MiniBarDf
scaledTotalAthletes <- rescale(countryMedals$Total.Athletes, to = c(100000,750000))



ui <- dashboardPage(
    dashboardHeader(title = "IST 687 - Olympics"),
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
                menuSubItem("Countries", tabName = "c_Countries", icon = icon("area-chart")),
                menuSubItem("Total Athletes", tabName = "c_Total_Athletes", icon = icon("area-chart"))
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
            setView(lat = 40.459152,
                    lng =  49.426570,
                    zoom = 4
            ) %>% 
            addCircles(lng=countryMedals$long, 
                       lat=countryMedals$lat,
                       radius=scaledTotalAthletes, 
                       popup=paste(countryMedals$team,"Total Athletes", as.character(countryMedals$Total.Athletes, " ")),
                       color = "#3462eb",
                       fillOpacity = 0.15,
                       stroke=FALSE
            )%>%
            addMinicharts(countryMedals$long,
                          countryMedals$lat,
                          type = "pie", 
                          chartdata = MiniBarDf, 
                          colorPalette = colors, 
                          legend = TRUE, 
                          legendPosition = "topright",
                          width = 20, height = 20)
                        
    })
}



shinyApp(ui, server)