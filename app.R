#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# app.R

# app.R
# app.R

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

# Load the cleaned weather dataset
weather_df <- read_csv("weather_data_cleaned.csv") %>%
  mutate(Date = as.Date(Date))

# UI
ui <- fluidPage(
  titlePanel("Mean Temperature Trend (2020–2024)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:",
                  choices = unique(weather_df$Region),
                  selected = unique(weather_df$Region)[1],
                  multiple = FALSE),
      uiOutput("station_ui"),
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(weather_df$Date),
                     end = max(weather_df$Date),
                     min = min(weather_df$Date),
                     max = max(weather_df$Date),
                     format = "yyyy-mm-dd")
    ),
    
    mainPanel(
      plotlyOutput("tempTrendPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dynamic station choices based on selected region
  output$station_ui <- renderUI({
    stations <- weather_df %>%
      filter(Region == input$region) %>%
      pull(Station) %>%
      unique()
    
    selectInput("station", "Select Station:",
                choices = stations,
                selected = stations[1])
  })
  
  # Plot
  output$tempTrendPlot <- renderPlotly({
    filtered_data <- weather_df %>%
      filter(
        Region == input$region,
        Station == input$station,
        Date >= input$date_range[1],
        Date <= input$date_range[2]
      ) %>%
      group_by(YearMonth = floor_date(Date, "month")) %>%
      summarise(MeanTemp = mean(`Mean Temperature (°C)`, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(filtered_data, aes(x = YearMonth, y = MeanTemp)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(aes(text = paste("Month:", YearMonth,
                                  "<br>Mean Temp:", round(MeanTemp, 2), "°C")),
                 color = "darkblue") +
      labs(
        title = "Mean Temperature Trend",
        x = "Month",
        y = "Mean Temperature (°C)"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the app
shinyApp(ui = ui, server = server)



