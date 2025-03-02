# Protocol communicate about work being done then
# Don't forget to pull at the beginning of a session and make sure you're working on the main branch.
# Then when you are done working and want to sink up you, "Commit, Pull and resolve any issues, then push"

# Shouldn't matter but just in case...
# For consistency lets name each file (in our own data file and in the code)
# As follows:
#### ACLED_Africa_Regions.csv
#### merged_data_sahel.gpkg

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
library(scales)

# Load conflict datasets
merged_data_sahel <- st_read(here("data", "merged_data_sahel.gpkg"))
acled_raw <- read_csv(here("data","ACLED_Africa_Regions.csv"))
acled = acled_raw |>
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude)) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(event_date = dmy(event_date))

# Define UI
ui <- navbarPage(
  "Sahelian Conflict & Climate Dashboard",
  theme = bs_theme(
    bg = "#f4f4f4", fg = "#222222", primary = "#005f73",
    base_font = font_google("Lato"), heading_font = font_google("Merriweather")
  ),
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel(
               h3("Data Sources"),
               p("This dashboard integrates data from the following sources:"),
               tags$ul(
                 tags$li(
                   "Armed Conflict Location & Event Data Project (ACLED). 2024. ACLED Version 2024 Dataset. Retrieved from ",
                   a("https://acleddata.com", href = "https://acleddata.com", target = "_blank"),
                   ".", style = "word-wrap: break-word; white-space: normal;"
                 ),
                 tags$li(
                   "Beguería, S., Vicente Serrano, S. M., Reig-Gracia, F., Latorre Garcés, B. (2023). SPEIbase v.2.9 [Dataset]. DIGITAL.CSIC. ",
                   a("doi:10.20350/digitalCSIC/15470", href = "https://doi.org/10.20350/digitalCSIC/15470", target = "_blank"),
                   ".", style = "word-wrap: break-word; white-space: normal;"
                 ),
                 tags$li(
                   "Runfola, D. et al. (2020). geoBoundaries: A global database of political administrative boundaries. PLoS ONE 15(4): e0231866. ",
                   a("https://doi.org/10.1371/journal.pone.0231866", href = "https://doi.org/10.1371/journal.pone.0231866", target = "_blank"),
                   ".", style = "word-wrap: break-word; white-space: normal;"
                 )
               ),
               br()
             ),
             mainPanel(
               h3("Welcome to the Sahelian Conflict & Drought Dashboard"),
               tags$div(
                 style = "text-align: justify;",
                 p("This dashboard explores the relationship between drought and conflict events in the Sahel region. Worsening environmental conditions have been linked to resource scarcity, food insecurity, and displacement, all of which may contribute to increased conflict and extremist group recruitment."),
                 p("We analyze historical precipitation, evapotranspiration data, and armed conflict events across the Sahel from 1997 to 2023 (2011 to 2023 for South Sudan), focusing on spatial and temporal relationships. Countries included in this analysis: Burkina Faso, Chad, Mali, Mauritania, Niger, Senegal, Sudan, South Sudan, Eritrea, Nigeria, and Cameroon."),
                 p("The dashboard integrates multiple geospatial datasets from Google Earth Engine (GEE) and conflict events from ACLED."),
                 tags$hr(),
                 h4("How to Use This Dashboard"),
                 tags$ul(
                   tags$li("Use the tabs above to navigate between sections."),
                   tags$li("Filter the data by country, year range, and other parameters."),
                   tags$li("The 'Conflict Map' tab displays conflict events on an interactive map."),
                   tags$li("The 'Conflict Trends' tab examines the relationship between drought and conflict."),
                   tags$li("The 'Climate Trends' tab visualizes climate patterns using the Standardized Precipitation-Evapotranspiration Index (SPEI)."),
                   tags$li("Data used in this dashboard is for demonstration purposes only.")
                 )
               )
             )
           )
  )
  ,
  tabPanel("Conflict Map",
           sidebarLayout(
             sidebarPanel(
               selectInput("country_map", "Select Country:", 
                           choices = sort(unique(merged_data_sahel$country)),
                           multiple = TRUE,
                           selected = "Burkina Faso"),
               selectInput("event_type", "Select Event Types:", choices = unique(acled$event_type),
                           multiple = TRUE,
                           selected = "Battles"),
               sliderInput("year", "Select Year Range:", min = 1980, max = 2023, value = c(1980, 2023), sep = ""),
               numericInput("fatal_min", "Minimum Fatalities", min = 0, max = 1000, value = 0),
               numericInput("fatal_max", "Maximum Fatalities", min = 0, max = 1000, value = 1000)
             ),
             mainPanel(
               leafletOutput("conflictMap")
             )
           )
  ),
  
  tabPanel("Conflict Trends",
           sidebarLayout(
             sidebarPanel(
               selectInput("country_con", "Select Country:", 
                           choices = sort(unique(merged_data_sahel$country)),
                           selected = "Burkina Faso"),
               checkboxGroupInput("spei_48_category", "Select SPEI Categorization:", 
                                  choices = c("Moderately Dry", "Very Dry", "Extremely Dry"),
                                  selected = c("Moderately Dry", "Very Dry", "Extremely Dry")),
               selectInput("time_lag", "4 Year Drought Lag:", 
                           choices = c("TRUE", "FALSE"),
                           selected = "TRUE")
             ),
             mainPanel(
               plotOutput("conflictTrendPlot")
             )
           )
  ),
  
  tabPanel("Climate Trends",
           sidebarLayout(
             sidebarPanel(
               selectInput("country_cc", "Select Country:", 
                           choices = sort(unique(merged_data_sahel$country)),
                           selected = "Burkina Faso"),
               sliderInput("year", "Select Year Range:", min = 1980, max = 2023, value = c(1980, 2023), sep = "")
             ),
             mainPanel(
               plotOutput("climatePlot")
             )
           )
  )
)


# Define Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$country_map)
    acled %>%
      filter(year(event_date) >= input$year[1],
             year(event_date) <= input$year[2],
             fatalities >= input$fatal_min,
             fatalities <= input$fatal_max,
             country %in% input$country_map,  # Allow multiple country selection
             event_type %in% input$event_type)
  })
  
  output$conflictMap <- renderLeaflet({
    
    
    
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
    spei_values <- merged_data_sahel |> 
      filter(country == input$country_cc, year >= input$year[1], year <= input$year[2]) |>
      group_by(year) |>
      summarise(avg_annual_spei = mean(spei_48_month)) |>
      ungroup()
    
    ggplot(spei_values, aes(x = year, y = avg_annual_spei)) +
      geom_col(aes(fill = avg_annual_spei)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Black dashed line at y = 0
      scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                           midpoint = 0,
                           limits = c(-2.33,2.33),
                           name = "SPEI") +
      labs(title = "SPEI Climate Trends", 
           x = "Year", 
           y = "Annual Mean 48-month SPEI") +
      theme_minimal()
  })
  
  output$conflictTrendPlot <- renderPlot({
    req(input$country_con)
    
    # If time lag is enabled, start of drought period is 4 years earlier
    lag_true_false <- ifelse(input$time_lag == "TRUE", 3, 0)
    
    # Prepare data for next steps
    conflict_drought <- merged_data_sahel |>
      filter(country == input$country_con) |> # Filter for selected country
      select(year, country, shape_name, conflict_events, conflict_events_original, spei_48_month, spei_48_category) |>
      group_by(year) |>
      summarise(total_conflict_events = sum(conflict_events_original, na.rm = TRUE),
                mean_spei = mean(spei_48_month),
                spei_48_category = first(spei_48_category)) |> # Sum number of conflicts and avg spei in each year
      ungroup()
    
    # Create drought period data frames manually for each threshold
    drought_moderate <- conflict_drought %>%
      filter(mean_spei < -1) %>%
      arrange(year) %>%
      mutate(group = cumsum(c(1, diff(year) > 1))) %>%
      group_by(group) %>%
      summarise(start = min(year) - lag_true_false, end = max(year) + 1, .groups = "drop") %>%
      mutate(new_group = cumsum(lag(end, default = first(end)) < start)) %>%
      group_by(new_group) %>%
      summarise(start = min(start), end = max(end), .groups = "drop") |>
      mutate(category = "Moderately Dry")
    
    drought_very <- conflict_drought %>%
      filter(mean_spei < -1.43) %>%
      arrange(year) %>%
      mutate(group = cumsum(c(1, diff(year) > 1))) %>%
      group_by(group) %>%
      summarise(start = min(year) - lag_true_false, end = max(year) + 1, .groups = "drop") %>%
      mutate(new_group = cumsum(lag(end, default = first(end)) < start)) %>%
      group_by(new_group) %>%
      summarise(start = min(start), end = max(end), .groups = "drop") |>
      mutate(category = "Very Dry")
    
    drought_extreme <- conflict_drought %>%
      filter(mean_spei < -1.82999) %>%
      arrange(year) %>%
      mutate(group = cumsum(c(1, diff(year) > 1))) %>%
      group_by(group) %>%
      summarise(start = min(year) - lag_true_false, end = max(year) + 1, .groups = "drop") %>%
      mutate(new_group = cumsum(lag(end, default = first(end)) < start)) %>%
      group_by(new_group) %>%
      summarise(start = min(start), end = max(end), .groups = "drop") |>
      mutate(category = "Extremely Dry")
    
    # Create normal period data frame
    near_normal <- conflict_drought %>%
      filter(mean_spei >= -1 & mean_spei <= 1) %>%
      arrange(year) %>%
      mutate(group = cumsum(c(1, diff(year) > 1))) %>%
      group_by(group) %>%
      summarise(start = min(year) - lag_true_false, end = max(year) + 1, .groups = "drop") %>%
      mutate(new_group = cumsum(lag(end, default = first(end)) < start)) %>%
      group_by(new_group) %>%
      summarise(start = min(start), end = max(end), .groups = "drop") |>
      mutate(category = "Near Normal")
    
    # Create wet period data frames using inverse thresholds
    wet_moderate <- conflict_drought %>%
      filter(mean_spei > 1) %>%
      arrange(year) %>%
      mutate(group = cumsum(c(1, diff(year) > 1))) %>%
      group_by(group) %>%
      summarise(start = min(year) - lag_true_false, end = max(year) + 1, .groups = "drop") %>%
      mutate(new_group = cumsum(lag(end, default = first(end)) < start)) %>%
      group_by(new_group) %>%
      summarise(start = min(start), end = max(end), .groups = "drop") |>
      mutate(category = "Moderately Wet")
    
    wet_severe <- conflict_drought %>%
      filter(mean_spei > 1.43) %>%
      arrange(year) %>%
      mutate(group = cumsum(c(1, diff(year) > 1))) %>%
      group_by(group) %>%
      summarise(start = min(year) - lag_true_false, end = max(year) + 1, .groups = "drop") %>%
      mutate(new_group = cumsum(lag(end, default = first(end)) < start)) %>%
      group_by(new_group) %>%
      summarise(start = min(start), end = max(end), .groups = "drop") |>
      mutate(category = "Severely Wet")
    
    wet_extreme <- conflict_drought %>%
      filter(mean_spei > 1.82999) %>%
      arrange(year) %>%
      mutate(group = cumsum(c(1, diff(year) > 1))) %>%
      group_by(group) %>%
      summarise(start = min(year) - lag_true_false, end = max(year) + 1, .groups = "drop") %>%
      mutate(new_group = cumsum(lag(end, default = first(end)) < start)) %>%
      group_by(new_group) %>%
      summarise(start = min(start), end = max(end), .groups = "drop") |>
      mutate(category = "Extremely Wet")
    
    # Combine selected thresholds based on user input
    if (length(input$spei_48_category) == 0) {
      drought_periods <- tibble(start = numeric(0), end = numeric(0), category = character(0))
    } else {
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
      labs(title = "Conflict Trends and Drought Periods",
           x = "Year",
           y = "Number of Conflicts",
           fill = "SPEI Category") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)



###########################