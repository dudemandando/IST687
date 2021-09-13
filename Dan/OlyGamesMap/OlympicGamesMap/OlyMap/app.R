library(shiny)
library(shinydashboard)
library(mapdeck)

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
        
        
        mapdeck( style = mapdeck_style("dark"), pitch = 45 ) %>%
            add_scatterplot(
                data = capitals
                , lat = "lat"
                , lon = "lon"
                , radius = 100000
                , fill_colour = "country"
                , layer_id = "scatter_layer"
                , tooltip = "capital"
            )
        
        ## using legend options
        mapdeck( style = mapdeck_style("dark"), pitch = 45 ) %>%
            add_scatterplot(
                data = capitals
                , lat = "lat"
                , lon = "lon"
                , radius = 100000
                , fill_colour = "lon"
                , stroke_colour = "lat"
                , layer_id = "scatter_layer"
                , tooltip = "capital"
                , legend = TRUE
                , legend_options = list( digits = 5 )
            )
        
        
        df <- read.csv(paste0(
            'https://raw.githubusercontent.com/uber-common/deck.gl-data/master/',
            'examples/3d-heatmap/heatmap-data.csv'
        ))
        
        df <- df[ !is.na(df$lng), ]
        
        mapdeck(style = mapdeck_style("dark"), pitch = 45 ) %>%
            add_scatterplot(
                data = df
                , lat = "lat"
                , lon = "lng"
                , layer_id = "scatter_layer"
                , stroke_colour = "lng"
            )
        
        ## as an sf object
        library(sfheaders)
        sf <- sfheaders::sf_point( df, x = "lng", y = "lat")
        
        mapdeck( style = mapdeck_style("dark"), pitch = 45 ) %>%
            add_scatterplot(
                data = sf
                , radius = 100
                , fill_colour = "country"
                , layer_id = "scatter_layer"
                , tooltip = "capital"
            )
        
        
        
        
        ######
        #mapdeck(token = key)
    })
    
}

shinyApp(ui, server)