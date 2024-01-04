# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)

# Read your data
# Assuming you have a data frame named 'data', adjust accordingly
# For example: data <- read.csv("your_data_file.csv")

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "My Shiny Dashboard"),
  dashboardSidebar(
    # Add any sidebar elements if needed
  ),
  dashboardBody(
    # Place the main content here
    fluidRow(
      # Add your visualizations and components here
      box(
        title = "Total Validations by Day",
        plotlyOutput("daily_plot")
      ),
      box(
        title = "Weekly Comparison",
        plotlyOutput("weekly_comparison")
      )
    )
  )
)

# Server definition
server <- function(input, output) {
  
  # Assuming 'data' is your dataset
  output$daily_plot <- renderPlotly({
    # Create a plot of total validations by day
    daily_data <- data %>%
      group_by(JOUR) %>%
      summarise(total_validations = sum(NB_VALD, na.rm = TRUE))
    
    plot_ly(data = daily_data, x = ~JOUR, y = ~total_validations, type = 'scatter', mode = 'lines+markers', name = 'Total Validations') %>%
      layout(title = "Total Validations by Day", xaxis = list(title = "Date"), yaxis = list(title = "Total Validations"))
  })
  
  output$weekly_comparison <- renderPlotly({
    # Create a plot comparing weekly validations
    weekly_data <- data %>%
      group_by(week = lubridate::week(JOUR), year = lubridate::year(JOUR)) %>%
      summarise(total_validations = sum(NB_VALD, na.rm = TRUE))
    
    plot_ly(data = weekly_data, x = ~week, y = ~total_validations, type = 'bar', name = 'Total Validations') %>%
      layout(title = "Weekly Validations Comparison", xaxis = list(title = "Week"), yaxis = list(title = "Total Validations"))
  })
}

# Run the application
shinyApp(ui, server)

