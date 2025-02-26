#Protocol
# communicate about work being done
#then
# Don't forget to pull at the beginning of a session and make sure you're working on the main branch.
#Then
# when you are done working and want to sink up you, "Commit, Pull and resolve any issues, then push"

#Shouldn't matter but just in case...
#For consistency lets name each file (in our own data file and in the code)
#As follows:

#GTD_2021Jan-June_1222dist.xlsx

#District_Geometries_Africa.csv

#Annual_SPEI_Africa_1980_2025.csv

#ACLED_Africa_Regions.csv

library(shiny)
library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(DT)
library(leaflet)
library(bslib)
library(leaflet.extras)
library(janitor)
library(sf)

# Load conflict datasets
merged_data_sahel <- st_read(here("data", "merged_data_sahel.gpkg"))

acled_raw <- read_csv(here("data","ACLED_Africa_Regions.csv"))

acled = acled_raw |>
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude)) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(event_date = dmy(event_date))

# Define UI
ui <- fluidPage(
  theme = bs_theme(
    bg = "#f4f4f4", fg = "#222222", primary = "#005f73",
    base_font = font_google("Lato"), heading_font = font_google("Merriweather")
  ),
  titlePanel(div(style = "color:#005f73; font-size: 24px;", "Sub-Saharan Africa Conflict & Climate Analysis")),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = NULL, multiple = TRUE, selected = "Burkina Faso"),
      sliderInput("year", "Select Year:", min = 1980, max = 2025, value = c(1980, 2025), sep = ""),
      selectInput("event_type", "Select Event Type:", choices = NULL, multiple = TRUE, selected = "Battles"),
      selectInput("spei_48_category", "SPEI Drought Threshold:", choices = NULL, multiple = TRUE),
      selectInput("time_lag", "4 Year Drought Lag:", choices = NULL),
      actionButton("update", "Update Data", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h3("Overview"),
                 p("This dashboard provides insights into conflict occurrences and their relationship with climate trends. 
                Explore the interactive map, data summary, and visualizations to understand patterns and correlations.")),
        tabPanel("Conflict Map", leafletOutput("conflictMap", height = 500)),
        tabPanel("Data Summary", DTOutput("dataTable")),
        tabPanel("Climate Trends", plotOutput("climatePlot")),
        tabPanel("Conflict Trends",
                 plotOutput("conflictTrendsPlot"),
                 p("This plot shows the number of conflicts per year in relation to drought periods."))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "country", 
                      choices = sort(unique(merged_data_sahel$country)))
  })
  
  observe({
    updateSelectInput(session, "event_type", 
                      choices = unique(acled$event_type),
                      selected = "Battles")
  })
  
  observe({
    updateSelectInput(session, "spei_48_category", 
                      choices = c("Moderately Dry", "Very Dry", "Extremely Dry"),
                      selected = "Very Dry")
  })
  
  observe({
    updateSelectInput(session, "time_lag",
                      choices = c("TRUE", "FALSE"),
                      selected = "FALSE")
  })
  
  filtered_data <- reactive({
    req(input$country)
    acled %>%
      filter(year(event_date) >= input$year[1],
             year(event_date) <= input$year[2],
             country %in% input$country,  # Allow multiple country selection
             event_type %in% input$event_type)
  })
  
  output$conflictMap <- renderLeaflet({
    req(nrow(filtered_data()) > 0)
    
    # Define custom icons for each event type
    event_icons <- awesomeIconList(
      Battles = makeAwesomeIcon(icon = "crosshairs", markerColor = "red", library = "fa"),
      Protests = makeAwesomeIcon(icon = "users", markerColor = "blue", library = "fa"),
      Riots = makeAwesomeIcon(icon = "bolt", markerColor = "orange", library = "fa"),
      Violence_against_civilians = makeAwesomeIcon(icon = "exclamation-triangle", markerColor = "darkred", library = "fa"),
      Explosions_Remote_violence = makeAwesomeIcon(icon = "bomb", markerColor = "lightgray", library = "fa"),
      Strategic_Developments = makeAwesomeIcon(icon = "flag", markerColor = "green", library = "fa")  # New icon
    )
    
    # Create a safe mapping function for event types
    event_type_map <- function(event) {
      case_when(
        event == "Violence against civilians" ~ "Violence_against_civilians",
        event == "Explosions/Remote violence" ~ "Explosions_Remote_violence",
        event == "Strategic developments" ~ "Strategic_Developments",
        TRUE ~ event  # Keep original name for single-word types
      )
    }
    
    # Apply mapping *before* filtering to avoid missing values
    filtered_data_mod <- filtered_data() %>%
      mutate(event_type_safe = event_type_map(event_type)) %>%
      filter(event_type_safe %in% event_type_map(input$event_type))  # Ensure filtering is correct
    
    leaflet(filtered_data_mod) %>%
      addTiles() %>%
      addAwesomeMarkers(
        lng = ~longitude, lat = ~latitude,
        icon = ~event_icons[event_type_safe],  # Use mapped safe event type
        clusterOptions = markerClusterOptions(
          spiderfyOnMaxZoom = TRUE,  
          showCoverageOnHover = FALSE,
          zoomToBoundsOnClick = TRUE,
          removeOutsideVisibleBounds = TRUE
        ),
        popup = ~paste0(
          "<b>Event Type:</b> ", event_type, "<br>",
          "<b>Event Date:</b> ", event_date, "<br>",
          "<b>Actor:</b> ", actor1, "<br>",
          "<b>Fatalities:</b> ", fatalities, "<br>",
          "<b>Notes:</b> ", notes
        )
      )
  })
  
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$climatePlot <- renderPlot({
    spei_values <- merged_data_sahel %>% filter(country == input$country, year >= input$year[1], year <= input$year[2])
    ggplot(spei_values, aes(x = year, y = spei_48_month)) +
      geom_line(color = "#005f73", size = 1) +
      labs(title = "SPEI Climate Trends", x = "Year", y = "SPEI Index") +
      theme_minimal()
  })
  
  output$conflictTrendsPlot <- renderPlot({
    req(input$country)
    
    # Define threshold values
    drought_thresholds <- c(
      "Moderately Dry" = -1,
      "Very Dry" = -1.5,
      "Extremely Dry" = -2
    )
    
    # If time lag is enabled, start of drought period is 4 years earlier
    lag_true_false <- ifelse(input$time_lag == "TRUE", 4, 0)
    
    # Prepare data for next steps
    conflict_drought <- merged_data_sahel |>
      filter(country == input$country) |> # Filter for selected country
      select(year, country, shape_name, conflict_events, conflict_events_original, spei_48_month, spei_48_category) |>
      group_by(year) |>
      mutate(total_conflict_events = sum(conflict_events_original, na.rm = TRUE)) |> # Sum number of conflicts in each year
      ungroup()
    
    # Create drought period data frames manually for each threshold
    drought_moderate <- conflict_drought %>%
      filter(spei_48_month < -1) %>%
      arrange(year) %>%
      mutate(group = cumsum(c(1, diff(year) > 1))) %>%
      group_by(group) %>%
      summarise(start = min(year) - lag_true_false, end = max(year), .groups = "drop") %>%
      mutate(new_group = cumsum(lag(end, default = first(end)) < start)) %>%
      group_by(new_group) %>%
      summarise(start = min(start), end = max(end), .groups = "drop") |>
      mutate(category = "Moderately Dry")
    
    drought_very <- conflict_drought %>%
      filter(spei_48_month < -1.5) %>%
      arrange(year) %>%
      mutate(group = cumsum(c(1, diff(year) > 1))) %>%
      group_by(group) %>%
      summarise(start = min(year) - lag_true_false, end = max(year), .groups = "drop") %>%
      mutate(new_group = cumsum(lag(end, default = first(end)) < start)) %>%
      group_by(new_group) %>%
      summarise(start = min(start), end = max(end), .groups = "drop") |>
      mutate(category = "Very Dry")
    
    drought_extreme <- conflict_drought %>%
      filter(spei_48_month < -2) %>%
      arrange(year) %>%
      mutate(group = cumsum(c(1, diff(year) > 1))) %>%
      group_by(group) %>%
      summarise(start = min(year) - lag_true_false, end = max(year), .groups = "drop") %>%
      mutate(new_group = cumsum(lag(end, default = first(end)) < start)) %>%
      group_by(new_group) %>%
      summarise(start = min(start), end = max(end), .groups = "drop") |>
      mutate(category = "Extremely Dry")
    
    # Combine selected thresholds based on user input
    if (length(input$spei_48_category) == 0) {
      drought_periods <- tibble(start = numeric(0), end = numeric(0), category = character(0))
    } else {
      # Combine selected thresholds based on user input
      drought_periods <- bind_rows(
        if ("Moderately Dry" %in% input$spei_48_category) drought_moderate else NULL,
        if ("Very Dry" %in% input$spei_48_category) drought_very else NULL,
        if ("Extremely Dry" %in% input$spei_48_category) drought_extreme else NULL
      )
    }
    
    # Assign colors to different drought levels
    drought_colors <- c(
      "Moderately Dry" = "orange",
      "Very Dry" = "tomato",
      "Extremely Dry" = "darkred"
    )
    
    # Plot conflict trends with multiple drought layers
    ggplot(conflict_drought, aes(x = year, y = total_conflict_events)) +
      geom_line() +
      geom_point() +
      geom_rect(data = drought_periods, 
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = category),
                alpha = 0.3, inherit.aes = FALSE) +
      scale_fill_manual(values = drought_colors) +
      geom_vline(data = drought_periods %>% filter(start == end), 
                 aes(xintercept = start, color = category),
                 alpha = 0.3, linewidth = 1) +
      scale_color_manual(values = drought_colors) +
      labs(title = "Conflict Trends and Drought Periods",
           x = "Year",
           y = "Number of Conflicts",
           fill = "Drought Severity") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)



###########################