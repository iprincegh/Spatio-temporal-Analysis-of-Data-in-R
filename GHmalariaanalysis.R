library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
library(dplyr)
library(shiny)
library(DT)
library(leaflet)
library(plotly)
library(spdep)
library(gstat)
library(sp)
library(spatialreg)

setwd("~/Desktop/R Project")

# Ghana shapefile
ghana_shapefile <- st_read("gha_admbnda_gss_20210308_SHP/gha_admbnda_adm1_gss_20210308.shp")
head(ghana_shapefile)
names(ghana_shapefile)

# Renaming columns for consistency
ghana_shapefile <- ghana_shapefile %>%
  rename(Region = ADM1_EN)

# Checking for valid geometries
if (!all(st_is_valid(ghana_shapefile))) {
  ghana_shapefile <- st_make_valid(ghana_shapefile)
}

# malaria incidence data
malaria_data <- read.csv("Subnational Unit-data.csv")
head(malaria_data)
names(malaria_data)

malaria_data$Name <- trimws(malaria_data$Name)  # Remove extra spaces

# intervention data
intervention_data <- read.csv("ghana_intervention.csv")

# treatment data
treatment_data <- read.csv("Treatment.csv")

# malaria data + intervention data
ghana_combined_data <- merge(malaria_data, intervention_data, 
                             by = c("ISO3", "Name", "Year"))

# Merge with treatment data
ghana_full_data <- merge(ghana_combined_data, treatment_data, 
                         by = c("ISO3", "Name", "Year"), all = TRUE)

str(ghana_full_data)

# shapefile + malaria data
ghana_map_data <- merge(ghana_shapefile, malaria_data, 
                        by.x = "Region", by.y = "Name", all.x = TRUE)

# Checking for NA values in the merged data
summary(ghana_map_data)
ghana_map_data <- ghana_map_data %>% filter(!is.na(Value))

# Merge spatial data with full data
ghana_sf <- ghana_shapefile %>%
  left_join(ghana_full_data, by = c("Region" = "Name"))

str(ghana_sf)

# Aggregating incidence rates by region and year
ghana_hotspots <- malaria_data %>%
  group_by(Name, Year) %>%
  summarize(average_incidence = mean(Value, na.rm = TRUE))

# Temporal Analysis: Aggregate data by year
temporal_trends <- malaria_data %>%
  group_by(Year) %>%
  summarize(mean_incidence = mean(Value, na.rm = TRUE))

# Plotting temporal trends
plotTrends <- ggplot(temporal_trends, aes(x = Year, y = mean_incidence)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Temporal Trends in Malaria Incidence",
       x = "Year",
       y = "Mean Incidence (Cases per Thousand)") +
  theme_minimal()

interactiveTrends <- ggplotly(plotTrends)
interactiveTrends

# Spatial Analysis: Subset data for 2015
ghana_map_2015 <- ghana_map_data %>%
  filter(Year == 2015)

# Plotting interactive malaria hotspots map for 2015
tmap_mode("view")
malaria_hotspots_map <- tm_shape(ghana_map_2015) +
  tm_polygons(
    col = "Value",
    palette = "Reds",
    title = "Malaria Incidence (2015)",
    border.alpha = 0.5
  ) +
  tm_borders() +
  tm_layout(
    title = "Malaria Hotspots in Ghana (2015)",
    legend.position = c("left", "bottom")
  )
malaria_hotspots_map

# Intervention year
intervention_year <- 2012

# Calculate averages before and after intervention
comparison <- ghana_combined_data %>%
  mutate(Period = ifelse(Year < intervention_year, "Before Intervention", "After Intervention")) %>%
  group_by(Name, Period) %>%
  summarize(Mean_Incidence = mean(Value.x, na.rm = TRUE),
            Mean_Intervention = mean(Value.y, na.rm = TRUE))
print(comparison)

# comparison data + spatial data
ghana_map_data <- ghana_sf %>%
  left_join(comparison, by = c("Region" = "Name"), relationship = "many-to-many")

# Plotting comparison
tmap_mode("plot")
tm_shape(ghana_map_data) +
  tm_polygons(
    col = "Mean_Incidence",
    title = "Mean Malaria Incidence (Cases per Thousand)",
    palette = "Reds",
    style = "jenks"
  ) +
  tm_facets(by = "Period") +
  tm_layout(
    main.title = "Effectiveness of Malaria Interventions in Ghana",
    legend.outside = TRUE
  )

# Spatial Autocorrelation
# Neighbors list
nb <- poly2nb(ghana_sf, queen = TRUE)

# Convert neighbors to spatial weights matrix
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Filter out NA values for Moran's I test
ghana_sf <- ghana_sf %>% filter(!is.na(Value.x))
values <- ghana_sf$Value.x

# Moran's I test
moran_test <- moran.test(values, lw, zero.policy = TRUE)
print(moran_test)

# Local Moran's I
local_moran <- localmoran(values, lw, zero.policy = TRUE)
ghana_sf$local_moran <- local_moran[, 1]

# Visualizing Local Moran's I
tmap_mode("view")
tm_shape(ghana_sf) +
  tm_polygons("local_moran", style = "quantile", palette = "-RdBu", midpoint = 0, title = "LISA (Local Moran's I)") +
  tm_layout(main.title = "Local Moran's I Visualization")

# Variogram Analysis
# Converting centroids to spatial object
ghana_sf$centroid <- st_centroid(ghana_sf$geometry)
ghana_sp <- as(ghana_sf, "Spatial")

# Empirical variogram
v.m <- variogram(Value.x ~ 1, data = ghana_sp)
plot(v.m, plot.numbers = TRUE, main = "Empirical Variogram")

# Fit variogram model
model_initial <- vgm(psill = 8000, model = "Exp", range = 200, nugget = 1500)
vm_fit <- fit.variogram(v.m, model = model_initial, fit.method = 6)
print(vm_fit)

# Plotting fitted variogram
plot(v.m, vm_fit, plot.numbers = TRUE, main = "Fitted Variogram Model")

# Spatial Lag Model
lag_model <- lagsarlm(Value.x ~ Value.y + Region, data = ghana_sf, listw = lw)
summary(lag_model)

# Visualize marginal effects
ghana_sf$marginal_effects <- lag_model$coefficients["Value.y"] * ghana_sf$Value.y
tm_shape(ghana_sf) +
  tm_fill("marginal_effects", palette = "Blues", title = "Effect of Intervention Coverage") +
  tm_borders() +
  tm_layout(title = "Spatial Influence of Intervention Coverage")

# Dashboard
ui <- fluidPage(
  titlePanel("Malaria Spatio-Temporal Dashboard"),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f7f7;
        font-family: 'Arial', sans-serif;
      }
      .scrollable-container {
        height: 300px;
        overflow-y: auto;
        border: 1px solid #ddd;
        padding: 10px;
        background-color: #fff;
      }
      .tab-content > .tab-pane {
        height: 800px;
        background-color: #fff;
        padding: 15px;
        border: 1px solid #ddd;
        border-radius: 5px;
      }
      .navbar-default .navbar-brand {
        color: #fff;
        background-color: #2c3e50;
      }
      .navbar-default .navbar-nav > li > a {
        color: #2c3e50;
      }
      .navbar-default .navbar-nav > .active > a {
        background-color: #18bc9c;
        color: #fff;
      }
      .well {
        background-color: #fff;
        border: 1px solid #ddd;
        border-radius: 5px;
      }
    "))
  ),
  
  verticalLayout(
    # Upper half: Visualizations and controls
    fluidRow(
      column(
        width = 3,
        wellPanel(
          h4("About the Dashboard"),
          p("This dashboard provides an interactive visualization of malaria incidence and intervention effectiveness in Ghana over time. Use the controls below to explore the data."),
          br(),
          h4("Controls"),
          sliderInput("year", "Select Year:", 
                      min = 2010, 
                      max = 2020, 
                      value = 2015, step = 1),
          selectInput("metric", "Select Metric:", 
                      choices = c("Malaria Incidence" = "Value.x", 
                                  "Effective Treatment" = "Value.y"), 
                      selected = "Value.x"),
          br(),
          h4("Descriptions"),
          p(strong("Interactive Map:"), "Displays malaria incidence or intervention effectiveness by region for the selected year."),
          p(strong("Malaria Hotspots:"), "Displays malaria hotspots for the selected year."),
          p(strong("Trend Analysis:"), "Shows temporal trends of the selected metric across all regions."),
          p(strong("Comparison Plot:"), "Compares average malaria incidence and treatment effectiveness before and after intervention."),
          p(strong("Fitted Variogram:"), "Displays the fitted variogram for spatial analysis."),
          p(strong("LISA Plot:"), "Visualizes Local Indicators of Spatial Association (LISA) for malaria incidence."),
          p(strong("Moran's I Test:"), "Tests for spatial autocorrelation in the selected metric for the chosen year."),
          p(strong("Spatial Lag Model:"), "Displays the results of a spatial lag model analyzing the relationship between malaria incidence and intervention coverage."),
          p(strong("Marginal Effects:"), "Visualizes the marginal effects of intervention coverage on malaria incidence.")
        )
      ),
      column(
        width = 9,
        tabsetPanel(
          tabPanel("Interactive Map", leafletOutput("map", height = "750px")),
          tabPanel("Malaria Hotspots", tmapOutput("malaria_hotspots", height = "750px")),
          tabPanel("Trend Analysis", plotlyOutput("interactive_trends", height = "750px")),
          tabPanel("Comparison Plot", plotOutput("comparison_plot", height = "750px")),
          tabPanel("Fitted Variogram", plotOutput("fitted_variogram", height = "750px")),
          tabPanel("LISA Plot", tmapOutput("lisa_plot", height = "750px")),
          tabPanel("Moran's I Test", verbatimTextOutput("moran_test")),
          tabPanel("Spatial Lag Model", verbatimTextOutput("spatial_lag")),
          tabPanel("Marginal Effects", plotlyOutput("marginal_effects", height = "750px"))
        )
      )
    ),
    
    # Lower half: Ghana Map Data Table
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("Ghana Map Data"),
          div(class = "scrollable-container",
              DTOutput("ghana_map_data_table"))
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  year_data <- reactive({
    req(input$year)
    ghana_map_data %>%
      filter(Year == as.numeric(input$year))
  })
  
  # Leaflet Map
  output$map <- renderLeaflet({
    req(year_data(), input$metric)
    
    pal <- colorNumeric("YlOrRd", domain = year_data()[[input$metric]], na.color = "transparent")
    
    leaflet(year_data()) %>%
      addTiles() %>% 
      addPolygons(
        color = "black", weight = 1, smoothFactor = 0.5,
        fillColor = ~pal(year_data()[[input$metric]]),
        fillOpacity = 0.7,
        popup = ~paste0("<b>Region:</b> ", Region, "<br>",
                        "<b>Year:</b> ", Year, "<br>",
                        "<b>", input$metric, ":</b> ", round(year_data()[[input$metric]], 2))
      ) %>%
      addLegend(pal = pal, values = year_data()[[input$metric]], title = input$metric, position = "bottomright")
  })
  
  # Rendering Interactive Trends Plot
  output$interactive_trends <- renderPlotly({
    plotly::ggplotly(interactiveTrends)  # Convert ggplot to interactive plotly
  })
  
  # Rendering Comparison Plot
  output$comparison_plot <- renderPlot({
    # Plot comparison data
    ggplot(comparison, aes(x = Period, y = Mean_Incidence, fill = Period)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Comparison of Malaria Incidence Before and After Intervention",
           x = "Period",
           y = "Mean Incidence (Cases per Thousand)") +
      theme_minimal()
  })
  
  # Rendering Fitted Variogram Plot
  output$fitted_variogram <- renderPlot({
    plot(v.m, vm_fit, plot.numbers = TRUE, main = "Fitted Variogram Model")
  })
  
  # Rendering Marginal Effects Map
  output$marginal_effects <- renderTmap({
    req(ghana_sf)
    
    if (!"marginal_effects" %in% colnames(ghana_sf)) {
      return(NULL)
    }
    
    tmap_mode("view")  # Interactive mode
    
    # Create the tmap visualization
    tm_shape(ghana_sf) +
      tm_polygons(
        col = "marginal_effects",
        palette = "YlGnBu",
        title = "Marginal Effects",
        border.alpha = 0.5
      ) +
      tm_borders() +
      tm_layout(
        title = "Spatial Distribution of Marginal Effects",
        legend.position = c("left", "bottom")
      )
  })
  
  # Render Malaria Hotspots Plot
  output$malaria_hotspots <- renderTmap({
    tmap_mode("plot")
    tm_shape(ghana_map_2015) +
      tm_polygons(
        col = "Value",
        palette = "Reds",
        title = "Malaria Incidence (2015)",
        border.alpha = 0.5
      ) +
      tm_borders() +
      tm_layout(
        title = "Malaria Hotspots in Ghana (2015)",
        legend.position = c("left", "bottom")
      )
  })
  
  # Render LISA Plot
  output$lisa_plot <- renderTmap({
    tmap_mode("plot")
    tm_shape(ghana_sf) +
      tm_polygons("local_moran", style = "quantile", palette = "-RdBu", midpoint = 0, title = "LISA (Local Moran's I)") +
      tm_layout(main.title = "Local Moran's I Visualization")
  })
  
  # Moran's I Test Output
  output$moran_test <- renderPrint({
    req(input$metric)
    
    year_data_filtered <- ghana_map_data %>%
      filter(Year == as.numeric(input$year))
    
    if (nrow(year_data_filtered) == 0 || any(is.na(year_data_filtered[[input$metric]]))) {
      return("Insufficient data for Moran's I test. Ensure data is available for the selected year and metric.")
    }
    
    nb <- poly2nb(year_data_filtered, queen = TRUE)
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    
    values <- year_data_filtered[[input$metric]]
    
    tryCatch({
      moran <- moran.test(values, lw, zero.policy = TRUE)
      print(moran)
    }, error = function(e) {
      return(paste("Error in Moran's I test:", e$message))
    })
  })
  
  # Spatial Lag Model Output
  output$spatial_lag <- renderPrint({
    if (!exists("lag_model")) {
      return("Spatial lag model not available.")
    }
    summary(lag_model)
  })
  
  # Ghana Map Data Table
  output$ghana_map_data_table <- renderDT({
    datatable(ghana_map_data, options = list(
      pageLength = 10,
      scrollX = TRUE,
      scrollY = "300px"
    ))
  })
}

# Running Dashboard
shinyApp(ui, server)