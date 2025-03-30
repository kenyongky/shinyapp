library(shiny)
library(shinyWidgets)
library(dplyr)
library(lubridate)
library(readr)
library(sf)
library(gstat)
library(terra)
library(tmap)

# Load boundary shape
mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>%
  st_transform(crs = 3414)

# Load weather data
weather_data <- read_csv("data/weather_data_cleaned.csv")
weather_data$Date <- as.Date(weather_data$Date)

# Extract station coordinates
station_coords <- weather_data %>%
  select(Station, Latitude, Longitude) %>%
  distinct()

# UI
ui <- fluidPage(
  titlePanel("Spatial Interpolation of Weather Variables"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("variable", "Select Variable:",
                  choices = c("Total Rainfall (mm)" = "Daily Rainfall Total (mm)",
                              "Mean Temperature (°C)" = "Mean Temperature (°C)",
                              "Maximum Temperature (°C)" = "Maximum Temperature (°C)",
                              "Minimum Temperature (°C)" = "Minimum Temperature (°C)")),
      
      dateRangeInput("date_range", "Select Date Range:",
                     start = as.Date("2024-01-01"),
                     end = as.Date("2024-12-31"),
                     min = as.Date("2020-01-01"),
                     max = as.Date("2024-12-31"),
                     format = "yyyy-mm-dd",
                     startview = "month"),
      
      selectInput("method", "Interpolation Method:",
                  choices = c("Inverse Distance Weighted (IDW)" = "idw",
                              "Kriging" = "kriging"),
                  selected = "idw"),
      
      conditionalPanel(
        condition = "input.method == 'idw'",
        sliderTextInput("nmax", "Number of Nearest Stations (nmax):",
                        choices = as.character(1:15),
                        selected = "5",
                        grid = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.method == 'kriging'",
        selectInput("kriging_model", "Variogram Model:",
                    choices = c("Spherical" = "Sph",
                                "Exponential" = "Exp",
                                "Gaussian" = "Gau"),
                    selected = "Sph"),
        
        checkboxInput("advanced", "Show Advanced Controls", FALSE),
        
        conditionalPanel(
          condition = "input.advanced == true",
          numericInput("nugget", "Nugget:", value = 0),
          numericInput("psill", "Partial Sill:", value = 10),
          numericInput("range", "Range:", value = 5000)
        )
      ),
      
      actionButton("generate", "Generate Map")
    ),
    mainPanel(
      tmapOutput("interpolationMap", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  interpolation_result <- eventReactive(input$generate, {
    
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    filtered_data <- weather_data %>%
      filter(Date >= start_date & Date <= end_date)
    
    monthly_summary <- filtered_data %>%
      group_by(Station) %>%
      summarise(MONTHVAL = if (input$variable == "Daily Rainfall Total (mm)") {
        sum(.data[[input$variable]], na.rm = TRUE)
      } else {
        mean(.data[[input$variable]], na.rm = TRUE)
      }, .groups = "drop")
    
    rfdata <- left_join(monthly_summary, station_coords, by = "Station")
    
    rfdata_sf <- st_as_sf(rfdata, coords = c("Longitude", "Latitude"), crs = 4326) %>%
      st_transform(crs = 3414)
    
    grid <- terra::rast(mpsz2019, nrows = 690, ncols = 1075)
    xy <- terra::xyFromCell(grid, 1:ncell(grid))
    coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = 3414)
    coop <- st_filter(coop, mpsz2019)
    
    if (input$method == "idw") {
      interp_model <- gstat(formula = MONTHVAL ~ 1,
                            locations = rfdata_sf,
                            nmax = as.numeric(input$nmax),
                            set = list(idp = 2))
      
    } else {
      vgm_model <- variogram(MONTHVAL ~ 1, rfdata_sf)
      
      if (input$advanced) {
        model <- vgm(psill = input$psill,
                     model = input$kriging_model,
                     range = input$range,
                     nugget = input$nugget)
      } else {
        model <- vgm(model = input$kriging_model)
      }
      
      fit <- fit.variogram(vgm_model, model = model)
      interp_model <- gstat(formula = MONTHVAL ~ 1,
                            locations = rfdata_sf,
                            model = fit)
    }
    
    interp_result <- predict(interp_model, coop)
    interp_result$x <- st_coordinates(interp_result)[, 1]
    interp_result$y <- st_coordinates(interp_result)[, 2]
    interp_result$pred <- interp_result$var1.pred
    
    raster <- terra::rasterize(interp_result, grid, field = "pred", fun = "mean")
    names(raster) <- "pred"
    
    range_title <- paste(format(start_date, "%d %b %Y"), "to", format(end_date, "%d %b %Y"))
    
    list(raster = raster,
         title = paste(toupper(input$method), "Interpolation of", range_title))
  })
  
  output$interpolationMap <- renderTmap({
    req(interpolation_result())
    
    raster <- interpolation_result()$raster
    title <- interpolation_result()$title
    palette <- if (input$variable == "Daily Rainfall Total (mm)") "Blues" else "Oranges"
    
    tmap_mode("plot")
    tm_shape(raster) +
      tm_raster(
        col = "pred",
        col.scale = tm_scale(values = palette),
        col_alpha = 0.8,
        col.legend = tm_legend(title = input$variable)
      ) +
      tm_shape(mpsz2019) +
      tm_borders() +
      tm_title(title) +
      tm_layout(legend.outside = TRUE)
  })
}

# Run app
shinyApp(ui, server)
