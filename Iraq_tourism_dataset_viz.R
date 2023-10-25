library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
# Load the data from the CSV file
data <- read.csv("places.csv")
# Define the UI for the Shiny app
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  titlePanel("Vineeth Krishna 20BDS0387"),
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Select a city:", unique(data$City))
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Map",
          leafletOutput("map")
        ),
        tabPanel(
          "Bar Chart",
          plotOutput("cityChart")
        ),
        tabPanel(
          "Scatter Plot",
          plotOutput("latLngPlot")
        ),
        tabPanel(
          "Heatmap",
          leafletOutput("heatmap")
        )
      )
    )
  )
)
# Define the server for the Shiny app
server <- function(input, output) {
  # Filter the data by the selected city
  filtered_data <- reactive({
    data[data$City == input$city, ]
  })
  
  # Create the map with markers for each place
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~`Placenames`)
  })
  
  # Create the bar chart showing the number of places in each city
  output$cityChart <- renderPlot({
    city_counts <- table(data$City)
    barplot(city_counts, main = "Number of places in each city",sub = "Vineeth Krishna
20BDS0387", xlab = "City", ylab = "Number of places")
  })
  
  # Create the scatter plot showing the relationship between latitude and longitude
  output$latLngPlot <- renderPlot({
    plot(data$Longitude, data$Latitude, xlab = "Longitude", ylab = "Latitude", main = "Scatter
plot of Latitude and Longitude", sub = "Vineeth Krishna 20BDS0387")
  })
  
  # Create the heatmap showing the density of places in each city
  output$heatmap <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addHeatmap(lng = ~Longitude, lat = ~Latitude, blur = 20, max = 0.5)
    #addHeatmapPlugin(lng = ~Longitude, lat = ~Latitude, blur = 20, max = 0.5)
    #addHeatmapOptions(lng = ~Longitude, lat = ~Latitude, blur = 20, max = 0.5)
    
    
  })
}
# Run the Shiny app
shinyApp(ui, server)
