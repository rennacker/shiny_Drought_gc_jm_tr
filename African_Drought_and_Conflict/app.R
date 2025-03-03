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
library(rnaturalearth)

# Load conflict datasets
merged_data_sahel <- st_read(here("data", "merged_data_sahel.gpkg"))
acled_raw <- read_csv(here("data","ACLED_Africa_Regions.csv"))
acled = acled_raw |>
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude)) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(event_date = dmy(event_date))

# Get Africa country boundaries for the small map
africa_countries <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Define UI
ui <- navbarPage(
  "Sahelian Conflict & Climate Dashboard",
  theme = bs_theme(
    bg = "#f4f4f4", fg = "#222222", primary = "#005f73",
    base_font = font_google("Lato"), 
    heading_font = font_google("Merriweather"),
    version = 5  # Use Bootstrap 5 for better responsiveness
  ),
  header = tags$head(
    tags$style(HTML("
    .navbar {
      background: url('background.jpg') no-repeat center center;
      background-size: cover;
      height: 120px;
      position: relative;
    }
    
    .navbar:before {
      content: '';
      position: absolute;
      top: 0; left: 0; width: 100%; height: 100%;
      background: rgba(0, 0, 0, 0.5);
    }

    .navbar .navbar-brand, .navbar-nav > li > a {
      color: white !important;
      position: relative;
      z-index: 1;
    }

    /* Ensure tabs are always visible */
    .navbar-nav {
      flex-direction: row !important;
      flex-wrap: nowrap;
    }
    
    /* Optional: Adjust tab spacing if needed */
    .navbar-nav .nav-item {
      margin-right: 15px;
    }
  ")),
    # Add viewport meta tag for better mobile responsiveness
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    
    # JavaScript to set initial window size
    tags$script("
      // Attempt to resize window on load
      window.onload = function() {
        if (window.outerWidth < 1200) {
          window.resizeTo(1200, window.outerHeight);
        }
      }
    ")
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
               selectInput("country_map", "Select Countries:", 
                           choices = sort(unique(merged_data_sahel$country)),
                           multiple = TRUE,
                           selected = "Burkina Faso"),
               selectInput("event_type", "Select Event Types:", choices = unique(acled$event_type),
                           multiple = TRUE,
                           selected = c("Battles", "Riots", "Protests", "Violence against civilians", "Explosions/Remote violence", "Strategic developments")),
               sliderInput("year", "Select Year Range:", min = 1997, max = 2023, value = c(1997, 2023), sep = ""),
               # Slider for selecting range of fatalities
               sliderInput("fatalities", "Select Fatality Range:",
                           min = 0, max = 5000, 
                           value = c(0, 5000), step = 10)
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
               fluidRow(
                 column(9, 
                        div(style = "padding-right: 0px;", # Reduce right padding
                            plotOutput("conflictTrendPlot"))
                 ),
                 column(3, 
                        div(style = "height: 200px; padding-left: 0px;", # Reduce left padding
                            plotOutput("africaLocationMap", height = "100%", width = "100%"))
                )
              )
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
               fluidRow(
                 column(9, 
                        div(style = "padding-right: 0px;", # Reduce right padding
                            plotOutput("climatePlot"))
                 ),
                 column(3, 
                        div(style = "height: 200px; padding-left: 0px;", # Reduce left padding
                            plotOutput("africaLMap", height = "100%", width = "100%"))
                 )
               )
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
             fatalities >= input$fatalities[1], fatalities <= input$fatalities[2],
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
          "<b>Event Date:</b> ", event_date, "<br>",
          "<b>Event Type:</b> ", event_type, "<br>",
          "<b>Event Sub-Type:</b> ", sub_event_type, "<br>",
          "<b>Actor:</b> ", actor1, "<br>",
          "<b>Fatalities:</b> ", fatalities, "<br>",
          "<b>Notes:</b> ", notes
        )
      )
    
  })
  
  # New output for fully responsive Africa map for climate trends NOTE: "input$country_cc" for CLIMATE TRENDS
  output$africaLMap <- renderPlot({
    req(input$country_cc)
    
    # Create a small map of Africa with the selected country highlighted
    africa_map <- ggplot() +
      geom_sf(data = africa_countries, fill = "lightgray", color = "white", size = 0.2) +
      geom_sf(data = africa_countries %>% 
                filter(name == input$country_cc | 
                         # Handle potential name discrepancies between datasets
                         tolower(name) == tolower(input$country_cc) |
                         admin == input$country_cc),
              fill = "black", color = "white") +
      # Make plot more compact by setting aspect ratio and expanding limits
      coord_sf(expand = FALSE) +
      theme_void() +  # Minimal theme with no axes, labels, or grid
      theme(
        panel.background = element_rect(fill = "#f4f4f4", color = NA),
        plot.background = element_rect(fill = "#f4f4f4", color = NA),
        plot.margin = margin(0, 0, 0, 0)  # Remove all margins
      )
    
    return(africa_map)
  }, bg = "#f4f4f4")
  
  
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
  
  # New output for fully responsive Africa map for conflict trends NOTE "input$country_con" for CONFLICT TRENDS
  output$africaLocationMap <- renderPlot({
    req(input$country_con)
    
    # Create a small map of Africa with the selected country highlighted
    africa_map <- ggplot() +
      geom_sf(data = africa_countries, fill = "lightgray", color = "white", size = 0.2) +
      geom_sf(data = africa_countries %>% 
                filter(name == input$country_con | 
                         # Handle potential name discrepancies between datasets
                         tolower(name) == tolower(input$country_con) |
                         admin == input$country_con),
              fill = "black", color = "white") +
      # Make plot more compact by setting aspect ratio and expanding limits
      coord_sf(expand = FALSE) +
      theme_void() +  # Minimal theme with no axes, labels, or grid
      theme(
        panel.background = element_rect(fill = "#f4f4f4", color = NA),
        plot.background = element_rect(fill = "#f4f4f4", color = NA),
        plot.margin = margin(0, 0, 0, 0)  # Remove all margins
      )
    
    return(africa_map)
  }, bg = "#f4f4f4")
  
  
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