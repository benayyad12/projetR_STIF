# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)

library(lubridate)

# Assuming you have a data frame named 'data', adjust accordingly
# For example: data <- read.csv("your_data_file.csv")

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "STIF Dashboard"),
  # dashboardSidebar section
  dashboardSidebar(
    dateRangeInput("reference_period", label = "Select Reference Period", start = "2022-01-01", end = "2022-12-31"),
    dateRangeInput("comparison_period", label = "Select Comparison Period", start = "2022-01-01", end = "2022-12-31"),
    selectizeInput("selected_station", label = "Select Station", choices = unique(data$LIBELLE_ARRET)[1:10], multiple = FALSE)
  )
  
  ,
  dashboardBody(
    
    fluidRow(
      box(
        title = "Total Validations by Day",
        plotlyOutput("daily_plot")
      ),
      box(
        title = "Weekly Comparison",
        plotlyOutput("weekly_comparison")
      )
    ),
    fluidRow(
      box(
        title = "Station Statistics",
        verbatimTextOutput("station_stats")
      ),
      box(
        leafletOutput("map")
      )
    )
  )
)

# Server definition
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(data_sf_wgs84) %>%
      addTiles() %>%
      addPolygons(
        color = ~ifelse(LIBELLE_ARRET == input$selected_station, "red", "blue"),
        fillColor = ~ifelse(LIBELLE_ARRET == input$selected_station, "red", "blue"),
        fillOpacity = 0.7,
        weight = 1,
        label = ~LIBELLE_ARRET, 
        popup = ~LIBELLE_ARRET) 
  })
  
  
  # Filter data based on user input
  filtered_data <- reactive({
    data %>%
      filter(JOUR >= input$reference_period[1] & JOUR <= input$reference_period[2]) %>%
      mutate(period = "Reference") %>%
      bind_rows(data %>%
                  filter(JOUR >= input$comparison_period[1] & JOUR <= input$comparison_period[2]) %>%
                  mutate(period = "Comparison"))
  })
  
  
  # Create a plot of total validations by day
  output$daily_plot <- renderPlotly({
    daily_data <- filtered_data() %>%
      group_by(JOUR, period) %>%
      summarise(total_validations = sum(NB_VALD, na.rm = TRUE))
    
    plot_ly(data = daily_data, x = ~JOUR, y = ~total_validations, color = ~period,
            type = 'scatter', mode = 'lines+markers', name = 'Total Validations') %>%
      layout(title = "Total Validations by Day", xaxis = list(title = "Date"), yaxis = list(title = "Total Validations"))
  })
  
  # Create a plot comparing weekly validations
  output$weekly_comparison <- renderPlotly({
    weekly_data <- filtered_data() %>%
      group_by(week = lubridate::week(JOUR), year = lubridate::year(JOUR), period) %>%
      summarise(total_validations = sum(NB_VALD, na.rm = TRUE))
    
    plot_ly(data = weekly_data, x = ~week, y = ~total_validations, color = ~period,
            type = 'bar', name = 'Total Validations') %>%
      layout(title = "Weekly Validations Comparison", xaxis = list(title = "Week"), yaxis = list(title = "Total Validations"))
  })
  
  # Provide statistics for the selected station
  output$station_stats <- renderPrint({
    selected_station_data <- filtered_data() %>%
      filter(LIBELLE_ARRET == input$selected_station)
    
    # Display relevant statistics or analysis for the selected station
    # Adjust this based on your specific analysis needs
    summary(selected_station_data$NB_VALD)
  })
  
  # Create a map with the selected station highlighted
  output$station_map <- renderPlotly({
    selected_station_data <- filtered_data() %>%
      filter(LIBELLE_ARRET == input$selected_station)
    
    # Create a map using plotly or leaflet, highlight the selected station
    # Adjust this based on your specific mapping needs
    # Example using plotly:
    plot_ly() %>%
      add_trace(
        type = 'scattermapbox',
        mode = 'markers',
        lon = ~selected_station_data$geometry$coordinates[1],
        lat = ~selected_station_data$geometry$coordinates[2],
        marker = list(symbol = 'circle', size = 10, color = 'red'),
        text = ~selected_station_data$LIBELLE_ARRET
      ) %>%
      layout(
        mapbox = list(
          style = 'open-street-map',
          center = list(lon = ~selected_station_data$geometry$coordinates[1],
                        lat = ~selected_station_data$geometry$coordinates[2]),
          zoom = 14
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )
  })
}

# Run the application
shinyApp(ui, server)
