library(shiny)
library(shinydashboard)
library(mapdeck)

##################################################################
# ------------      Begin Load and Clean Data        ------------
##################################################################
#Created a function to Check for NA values
checkForNaValuesInColumn <- function(vec, colname){
    numberOfNas <-length(vec[is.na(vec)])
    print(colname)
    print(numberOfNas)
}

#read the CSV, Create a Data Frame from each Dataset
athleteEvents <- data.frame(read.csv("C:\\Users\\danbu\\Desktop\\OlympicData\\athlete_events.csv"))
nocRegions <- data.frame(read.csv("C:\\Users\\danbu\\Desktop\\OlympicData\\noc_regions.csv"))

#Grab the column names for reference
athleteColumnNames <- colnames(athleteEvents) 
regionColnames <- colnames(nocRegions)

#Replace NA values for Medals with "None", we can assume a NA == no medal, however we can't assume the same for any other values
athleteEvents$Medal[is.na(athleteEvents$Medal)] <- "None"

#check For NA values
checkForNaValuesInColumn(athleteEvents$Name, "Name")
checkForNaValuesInColumn(athleteEvents$Sex, "Sex")
checkForNaValuesInColumn(athleteEvents$Age, "Age")
checkForNaValuesInColumn(athleteEvents$Height, "Height")
checkForNaValuesInColumn(athleteEvents$Weight, "Weight")
checkForNaValuesInColumn(athleteEvents$Team, "Team")
checkForNaValuesInColumn(athleteEvents$NOC, "NOC")
checkForNaValuesInColumn(athleteEvents$Games, "Games")
checkForNaValuesInColumn(athleteEvents$Year, "Year")
checkForNaValuesInColumn(athleteEvents$Season, "Season")
checkForNaValuesInColumn(athleteEvents$Sport, "Sport")
checkForNaValuesInColumn(athleteEvents$City, "City")
checkForNaValuesInColumn(athleteEvents$Event, "Event")
checkForNaValuesInColumn(athleteEvents$Medal, "Medal")




#Omit all NA rows within the Athlete Events Data Frame

athletes1 <- na.omit(athleteEvents)



print("Number of Omitted Rows")
print(length(athleteEvents$ID)-length(athletes1$ID))

#Reivew the Data Frame
str(athletes1)


#Convert CM to Feet (Decimal)
athletes1$Height <- athletes1$Height / 30.48

#Convert KG to lbs

athletes1$Weight <- athletes1$Weight * 2.2

#View Distribution
hist(athletes1$Height)
plot(athletes1$Height)
hist(athletes1$Weight)
plot(athletes1$Weight)
summary(athletes1$Height)
summary(athletes1$Weight)




countries <- read.csv("C:\\Users\\danbu\\Desktop\\OlympicData\\allCountries.csv")

#Clean up Country Name Strings
library(stringr)

#Match up Countries & Athletes we have locations for, with the change in country names this will affect the data
matchedCoAthIndex<- athletes1[match(athletes1$Team, countries$name),]
matchedCoAthIndex <- na.omit(matchedCoAthIndex)

#Get the Athletes who have never medal'd,We know that countries can medal and not medal one year so we 
#must find countries which have not recieved a medal at all
countries <- unique(athletes1$Team)
countries <- lapply(countries, sub, pattern = '\\-1', replacement = '')
countries <- lapply(countries, sub, pattern = '\\-2', replacement = '')
countries <- lapply(countries, sub, pattern = '\\-3', replacement = '')
countryMedalSum <- data.frame()

for(i in 1:length(countries)){
    countryMedalSum[i,1] <- countries[i]
    countryMedalSum[i,2] <- nrow(athletes1[which(athletes1$Team == countries[i] & athletes1$Medal =="Gold"),])
    countryMedalSum[i,3] <- nrow(athletes1[which(athletes1$Team == countries[i] & athletes1$Medal =="Silver"),])
    countryMedalSum[i,4] <- nrow(athletes1[which(athletes1$Team == countries[i] & athletes1$Medal =="Bronze"),])
    countryMedalSum[i,5] <- nrow(athletes1[which(athletes1$Team == countries[i] & athletes1$Medal =="None"),])
    #Get and Add the sum for each medal
    countryMedalSum[i,6] <- sum(
        nrow(athletes1[which(athletes1$Team == countries[i] & athletes1$Medal =="Gold"),]),
        nrow(athletes1[which(athletes1$Team == countries[i] & athletes1$Medal =="Silver"),]),
        nrow(athletes1[which(athletes1$Team == countries[i] & athletes1$Medal =="Bronze"),]),
        nrow(athletes1[which(athletes1$Team == countries[i] & athletes1$Medal =="None"),])
    )
}



colnames(countryMedalSum) <- c("team", "Gold", "Silver", "Bronze", "None", "Total Athletes") 

countryGeo <- read.csv("C:\\Users\\danbu\\Desktop\\OlympicData\\allCountries.csv")
countryMedalSum <- countryMedalSum[match(countryGeo$name,countryMedalSum$team),]
countryMedalSum <-na.omit(countryMedalSum)

##################################################################
# ------------      End Load and Clean Data        ------------
##################################################################


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