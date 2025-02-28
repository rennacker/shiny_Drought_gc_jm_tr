# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Assuming merged_data_sahel is pre-loaded and contains necessary columns (e.g., country, year, spei_48_month)
# merged_data_sahel <- st_read("path_to_your_data.gpkg")  # If not pre-loaded

# Define UI
ui <- navbarPage("Conflict & Climate Dashboard",
                 
                 tabPanel("Climate Trends & Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("country_cc", "Select Country:", 
                                          choices = sort(unique(merged_data_sahel$country)),
                                          selected = "Burkina Faso"),
                              sliderInput("year", "Select Year Range:", 
                                          min = 1980, max = 2025, 
                                          value = c(1980, 2025), sep = "")
                            ),
                            mainPanel(
                              # Stack the plots vertically in the main panel
                              verticalLayout(
                                plotOutput("climatePlot"),  # Display climate trend plot
                                plotOutput("countryMap", height = "400px")    # Display static map with highlighted country
                              )
                            )
                          )
                 )
)

# Define server logic
server <- function(input, output, session) {
  
  # Create a reactive expression to filter data based on user input (country and year range)
  filtered_data <- reactive({
    merged_data_sahel %>%
      filter(country == input$country_cc, 
             year >= input$year[1], year <= input$year[2])
  })
  
  # Render the plot with climate trends (SPEI index)
  output$climatePlot <- renderPlot({
    # Get filtered data
    spei_values <- filtered_data() %>%
      group_by(year) %>%
      summarise(avg_annual_spei = mean(spei_48_month, na.rm = TRUE)) %>%
      ungroup()
    
    # Create the plot using ggplot
    ggplot(spei_values, aes(x = year, y = avg_annual_spei)) +
      geom_col(aes(fill = avg_annual_spei)) +  # Bar plot
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Horizontal dashed line at y = 0
      scale_fill_gradient2(low = "red", mid = "white", high = "blue",  # Color gradient
                           midpoint = 0,
                           limits = c(-2.1, 2),
                           name = "SPEI Index") +
      labs(title = "SPEI Climate Trends", 
           x = "Year", 
           y = "Mean Annual SPEI Index") +
      theme_minimal()
  })
  
  # Render the simple static map with the highlighted country using ggplot
  output$countryMap <- renderPlot({
    # Load Africa map data using rnaturalearth
    africa <- ne_countries(continent = "Africa", returnclass = "sf")
    
    # Get the geometry of the selected country
    selected_country <- africa %>%
      filter(name == input$country_cc) %>%
      st_geometry() %>%
      st_union()  # Combine multiple geometries into one (in case of MULTIPOLYGON)
    
    # Create the plot with all of Africa and highlight the selected country
    ggplot() +
      geom_sf(data = africa, fill = "lightgrey", color = "black", size = 0.5) +  # All of Africa
      geom_sf(data = selected_country, fill = "purple", color = "black", size = 1.5) +  # Highlight selected country
      theme_minimal() +
      labs(title = paste(input$country_cc)) +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid = element_blank())
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)