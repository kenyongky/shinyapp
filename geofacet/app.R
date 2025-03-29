library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(geofacet)
library(readr)
library(tidyr)

# Set locale to ensure month abbreviations match month.abb
Sys.setlocale("LC_TIME", "C")

# Load dataset
weather_data <- read_csv("weather_data_cleaned.csv")

# Custom geofacet grid
custom_grid <- data.frame(
  name = c("Admiralty", "Sembawang", "Seletar", "Pulau Ubin",
           "Ang Mo Kio", "Newton", "Tai Seng", "Paya Lebar",
           "Choa Chu Kang (South)", "Jurong (West)", "Clementi",
           "Jurong Island", "Tuas South", "Sentosa Island",
           "Pasir Panjang", "East Coast Parkway", "Changi"),
  code = c("Admiralty", "Sembawang", "Seletar", "Pulau Ubin",
           "Ang Mo Kio", "Newton", "Tai Seng", "Paya Lebar",
           "Choa Chu Kang (South)", "Jurong (West)", "Clementi",
           "Jurong Island", "Tuas South", "Sentosa Island",
           "Pasir Panjang", "East Coast Parkway", "Changi"),
  row = c(2, 1, 2, 2, 3, 3, 4, 3, 2, 3, 4, 5, 4, 5, 4, 4, 3),
  col = c(3, 3, 4, 5, 4, 3, 4, 5, 2, 2, 2, 2, 1, 3, 3, 5, 6),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("Singapore Weather Geofacet Visualisation (2020–2024)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Select Metric:",
                  choices = c("Yearly Total Rainfall",
                              "Yearly Mean Temperature",
                              "Mean Monthly Temperature",
                              "Monthly Total Rainfall",
                              "Dry vs Wet Days Per Month")),
      conditionalPanel(
        condition = "input.metric != 'Yearly Total Rainfall' && input.metric != 'Yearly Mean Temperature'",
        selectInput("year", "Select Year:", choices = 2020:2024)
      )
    ),
    mainPanel(
      plotOutput("geofacetPlot", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$geofacetPlot <- renderPlot({
    
    weather_data_clean <- weather_data %>%
      mutate(Date = as.Date(Date),
             Year = year(Date),
             Month = month(Date, label = TRUE, abbr = TRUE))
    
    plot_data <- switch(input$metric,
                        
                        "Yearly Total Rainfall" = weather_data_clean %>%
                          group_by(Station, Region, Year) %>%
                          summarise(Value = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop") %>%
                          ggplot(aes(x = Year, y = Value, color = Region)) +
                          geom_line(size = 1) +
                          labs(y = "Total Rainfall (mm)", title = "Yearly Total Rainfall"),
                        
                        "Yearly Mean Temperature" = weather_data_clean %>%
                          group_by(Station, Region, Year) %>%
                          summarise(Value = mean(`Mean Temperature (°C)`, na.rm = TRUE), .groups = "drop") %>%
                          ggplot(aes(x = Year, y = Value, color = Region)) +
                          geom_line(size = 1) +
                          labs(y = "Mean Temp (°C)", title = "Yearly Mean Temperature"),
                        
                        "Mean Monthly Temperature" = weather_data_clean %>%
                          filter(Year == input$year) %>%
                          group_by(Station, Region, Month) %>%
                          summarise(Value = mean(`Mean Temperature (°C)`, na.rm = TRUE), .groups = "drop") %>%
                          ggplot(aes(x = Month, y = Value, color = Region, group = 1)) +
                          geom_line(size = 1) +
                          labs(y = "Mean Temp (°C)", title = paste("Mean Monthly Temp -", input$year)),
                        
                        "Monthly Total Rainfall" = weather_data_clean %>%
                          filter(Year == input$year) %>%
                          group_by(Station, Region, Month) %>%
                          summarise(Value = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop") %>%
                          ggplot(aes(x = Month, y = Value, fill = Region)) +
                          geom_col() +
                          labs(y = "Rainfall (mm)", title = paste("Monthly Total Rainfall -", input$year)),
                        
                        "Dry vs Wet Days Per Month" = weather_data_clean %>%
                          filter(Year == input$year) %>%
                          mutate(
                            Month_Label = factor(format(floor_date(Date, "month"), "%b"), levels = month.abb),
                            RainType = ifelse(`Daily Rainfall Total (mm)` > 0, "Wet", "Dry")
                          ) %>%
                          group_by(Station, Region, Month_Label, RainType) %>%
                          summarise(Days = n(), .groups = "drop") %>%
                          complete(
                            Station, Region,
                            Month_Label = factor(month.abb, levels = month.abb),
                            RainType = c("Dry", "Wet"),
                            fill = list(Days = 0)
                          ) %>%
                          ggplot(aes(x = Month_Label, y = Days, fill = RainType)) +
                          geom_col() +
                          scale_fill_manual(values = c("Dry" = "skyblue", "Wet" = "steelblue")) +
                          labs(
                            title = paste("Dry vs Wet Days Per Month by Station (", input$year, ")", sep = ""),
                            x = "Month", y = "Number of Days", fill = "Day Type"
                          )
    )
    
    plot_data +
      facet_geo(~ Station, grid = custom_grid) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
        plot.title = element_text(face = "bold", size = 14)
      ) +
      labs(x = NULL)
  })
}

# Run the app
shinyApp(ui, server)


