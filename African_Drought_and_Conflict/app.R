library(shiny)
library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(DT) # for interactive tables
library(leaflet)
library(bslib)
library(leaflet.extras)
library(janitor)
library(sf)# for spatial data handling
library(scales)
library(rnaturalearth)
library(fixest)  # for fixed effects Poisson regression
library(ggplot2)  # for creating plots
library(viridis)  # for viridis color palette
library(broom)  # for tidy model output
library(gt)  # for nice tables


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

# Load the dataset
sahel_pop_with_fill <- read_csv(here("data", "Sahel_Pop_Con_SPEI_Regression_NA.csv"))

sahel_pop_with_fill$country <- str_to_title(sahel_pop_with_fill$country)

sahel_pop_with_fill <- sahel_pop_with_fill %>%
  mutate(population = population / 100000)

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
                 ),
                 tags$li(
                   "WorldPop. (2025). Open spatial demographic data and research. University of Southampton. ",
                   a("https://www.worldpop.org/", href = "https://www.worldpop.org/", target = "_blank"),
                   ".", style = "word-wrap: break-word; white-space: normal;"
                 )
               )
             ),
             mainPanel(
               h3("Welcome to the Sahelian Conflict & Drought Dashboard"),
               tags$div(
                 style = "text-align: justify;",
                 p("This dashboard explores the relationship between drought and conflict events in the Sahel region. Worsening environmental conditions have been linked to resource scarcity, food insecurity, and displacement, all of which may contribute to increased conflict and extremist group recruitment."),
                 p("We analyze historical precipitation & evapotranspiration data, population data, and armed conflict events across the Sahel from 1997 to 2023 (2011 to 2023 for South Sudan), focusing on spatial and temporal relationships. Countries included in this analysis: Burkina Faso, Chad, Mali, Mauritania, Niger, Senegal, Sudan, South Sudan, Eritrea, Nigeria, and Cameroon."),
                 p("The dashboard integrates multiple geospatial datasets from Google Earth Engine (GEE) and conflict events from ACLED."),
                 tags$hr(),
                 h4("How to Use This Dashboard"),
                 tags$ul(
                   tags$li("Use the tabs above to navigate between sections."),
                   tags$li("Filter the data by country, year range, and other parameters."),
                   tags$li("The 'Conflict Map' tab displays conflict events on an interactive map."),
                   tags$li("The 'Conflict Trends' tab examines the relationship between drought and conflict."),
                   tags$li("The 'Climate Trends' tab visualizes climate patterns using the Standardized Precipitation-Evapotranspiration Index (SPEI)."),
                   tags$li("The 'Poisson Regression' tab performs a Poisson regression to analyze how drought severity correlates with conflict frequency while controlling for population."),
                   tags$li("Data used in this dashboard is for demonstration purposes only.")
                 )
               )
             )
           )
  ),
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
               sliderInput("year_conflict", "Select Year Range:", min = 1997, max = 2023, value = c(1997, 2023), sep = ""),
               # Slider for selecting range of fatalities
               sliderInput("fatalities", "Select Fatality Range:",
                           min = 0, max = 5000, 
                           value = c(0, 5000), step = 10),
               
               # Add a divider
               tags$hr(),
               
               # "About This Analysis" 
               tags$div(
                 style = "margin-top: 15px;",
                 tags$h4("Conflict Map"),
                 tags$p("The Conflict Map tab displays an interactive map showing conflict events across the Sahel region. Use the filters to select countries, event types, and year range, and adjust the fatality range to visualize the spatial distribution of conflicts. The map allows for detailed exploration of conflict events and their locations.")
               )
             ),
             
             mainPanel(
               # Adjust the layout to make the map take up more space
               fluidRow(
                 column(12, 
                        leafletOutput("conflictMap", height = "800px")  # Increase the height of the map
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
               sliderInput("year_climate", "Select Year Range:", min = 1997, max = 2023, value = c(1997, 2023), sep = ""),
               
               # Add a divider
               tags$hr(),
               
               # Added example text with the same format as "About This Analysis"
               tags$div(
                 style = "margin-top: 15px;",
                 tags$h4("Climate Trends Analysis"),
                 tags$p("This tab visualizes climate patterns in the Sahel region using the Standardized Precipitation-Evapotranspiration Index (SPEI)."),
                 tags$p("The figure on this tab illustrates the trends in drought conditions over different time periods, allowing you to understand the long-term climate variability."),
                 tags$p("Use the filters to select a specific country and year range to customize the analysis. The SPEI values are shown for various timescales, representing long-term (48-Month SPEI) drought conditions."),
                 tags$p("This visualization helps to identify periods of drought and their severity, providing insights into general climatic trends in the region.")
               )
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
                           selected = "TRUE"),
               
               # Divider for visual clarity
               tags$hr(),
               
               # Informative text about the visualization
               tags$div(
                 style = "margin-top: 15px;",
                 tags$h4("Conflict Trends Analysis"),
                 tags$p("This tab visualizes the temporal relationship between drought conditions 
                and conflict events in your selected country. The timeline displays:"),
                 tags$ul(
                   tags$li("Conflict events (black line) across available years"),
                   tags$li("Colored overlays indicating drought periods based on your selected SPEI thresholds"),
                   tags$li("Optional 4-year lag to show delayed drought effects on conflict")
                 ),
                 tags$p("Different drought severities are color-coded from orange (moderately dry) 
                to dark red (extremely dry). This visualization helps identify potential 
                connections between drought periods and subsequent increases in conflict activity.")
               )
             ),
             
             mainPanel(
               fluidRow(
                 column(8,  # Improved column width balance
                        div(style = "padding-right: 10px;", 
                            plotOutput("conflictTrendPlot"))
                 ),
                 column(4, 
                        div(style = "height: 200px; padding-left: 10px;", 
                            plotOutput("africaLocationMap", height = "100%", width = "100%"))
                 )
               )
             )
           )
  ),
  
  
  tabPanel("Poisson Regression",
           
           sidebarLayout(
             sidebarPanel(
               # Option to select the country
               selectInput("country_r", "Select Country:",
                           choices = unique(sahel_pop_with_fill$country),
                           selected = unique(sahel_pop_with_fill$country)[1]),
               
               # Select the year range
               sliderInput("year_range", "Select Year Range:",
                           min = 2000,
                           max = 2020,
                           value = c(2000, 2020), 
                           step = 1,
                           sep = ""),
               
               # Select the spei variable - only numeric month options
               selectInput("spei_var", "Select SPEI Variable:",
                           choices = c("1 Month" = "spei_01_month", 
                                       "12 Months" = "spei_12_month", 
                                       "24 Months" = "spei_24_month", 
                                       "48 Months" = "spei_48_month"), 
                           selected = "spei_01_month"),
               
               # Add a divider
               tags$hr(),
               
               # Add a text section below the controls
               tags$div(
                 style = "margin-top: 15px;",
                 tags$h4("Poisson Regression"),
                 tags$p("The statistical model uses Poisson regression (fepois) to analyze how SPEI and population may predict coflict frequency."),
                 tags$p("This Analysis uses only SPEI and population data for a set amount of years. The analysis could be bolstered through by including multiple countries at once, adding a variable for year, including multiple SPEI-timeframes at once, and adding any number of other explanatory variables.")
               )
             ),
             
             mainPanel(
               # Move the total population display here
               tags$div(
                 style = "margin-bottom: 15px; font-weight: bold; padding-top: 5px;",
                 textOutput("total_population")
               ),
               # Top row with model results and Africa map
               fluidRow(
                 column(8, h6("Bold p-values are significant at p < 0.05"),
                        div(style = "padding-right: 0px;", # Reduce right padding
                            gt_output("model_table")
                        )
                 ),
                 # Right column for Africa map
                 column(4,
                        h4(NULL),
                        div(style = "height: 200px; padding-left: 0px;", # Reduce left padding
                            plotOutput("africa_map", height = "100%", width = "100%")
                        )
                 )
               ),
               
               # Plot to show the relationship between conflict and SPEI
               h4(NULL),
               plotOutput("regression_plot", height = "400px")
             )
           )
  )
)

#################git config --global --unset http.proxy######################## This was used to fix a fatal error at some point, Removed by Travis. 

# Define Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$country_map)
    acled %>%
      filter(year(event_date) >= input$year_conflict[1],
             year(event_date) <= input$year_conflict[2],
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
      filter(country == input$country_cc, year >= input$year_climate[1], year <= input$year_climate[2]) |>
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
  
  # Reactive expression for filtered data based on selected year range and country
  reactive_data <- reactive({
    data <- sahel_pop_with_fill %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
      filter(country == input$country_r) %>%
      # Select columns
      select(year, country, conflict_events, population, all_of(input$spei_var)) %>%
      drop_na()  # Remove NA rows for the selected columns
    
    return(data)
  })

  
  # Reactive expression for the Poisson regression model
  regression_model <- reactive({
    data <- reactive_data()
    
    # Check if we have enough data for regression
    if(nrow(data) < 3) {
      return(NULL)
    }
    
    spei_var_name <- input$spei_var
    
    # Create the formula object based on the selected SPEI variable
    formula_obj <- as.formula(paste("conflict_events ~", spei_var_name, "+ population"))
    
    # Perform the Poisson regression using the fepois function
    tryCatch({
      model <- fepois(formula_obj, data = data)
      return(model)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Calculate pseudo R²
  pseudo_r_squared <- reactive({
    model <- regression_model()
    
    if(is.null(model)) {
      return(NA)
    }
    
    # Get fitted values
    fitted_values <- fitted(model)
    
    # Get observed values
    observed <- reactive_data()$conflict_events
    
    # Calculate null model (intercept only) prediction (mean of observed)
    null_pred <- rep(mean(observed), length(observed))
    
    # Calculate deviances
    null_deviance <- sum((observed - null_pred)^2)
    residual_deviance <- sum((observed - fitted_values)^2)
    
    # Calculate McFadden's pseudo R²
    r_squared <- 1 - (residual_deviance / null_deviance)
    
    return(r_squared)
  })
  
  # Output the model as a clean gt table using broom's tidy function
  output$model_table <- render_gt({
    model <- regression_model()
    r_squared <- pseudo_r_squared()
    
    # If model is NULL, return a simple message dataframe
    if(is.null(model)) {
      data.frame(
        term = "Insufficient data or model error",
        estimate = NA,
        p.value = NA
      ) %>%
        gt() %>%
        tab_header(title = "Model Results")
    } else {
      # Use broom's tidy function to get clean coefficient table
      tidy_model <- tidy(model, conf.int = TRUE)
      
      # Format term names for better readability
      tidy_model <- tidy_model %>%
        mutate(term = case_when(
          term == "(Intercept)" ~ "Intercept",
          grepl("spei_", term) ~ paste("SPEI", gsub("spei_|_month", "", term)),
          term == "population" ~ "Population: per 100,000",
          TRUE ~ term
        ))
      
      # Create gt table
      tbl <- tidy_model %>%
        select(term, estimate, std.error, p.value) %>%
        gt() %>%
        tab_header(title = "Regression Results") %>%
        fmt_number(
          columns = c(estimate, std.error, p.value),
          decimals = 4
        ) %>%
        cols_label(
          term = "Term",
          estimate = "Estimate",
          std.error = "Std. Error",
          p.value = "p-value"
        ) %>%
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(
            columns = p.value,
            rows = p.value < 0.05
          )
        ) 
      
      # Add pseudo R² as a source note
      if(!is.na(r_squared)) {
        tbl <- tbl %>%
          tab_source_note(
            source_note = md(paste0("Pseudo R² :   ", round(r_squared, 4)))
          )
      }
      
      return(tbl)
    }
  })
  
  # Output the Africa map with selected country highlighted
  output$africa_map <- renderPlot({
    req(input$country_r)
    
    # Create a small map of Africa with the selected country highlighted
    africa_map <- ggplot() +
      geom_sf(data = africa_countries, fill = "lightgray", color = "white", size = 0.2) +
      geom_sf(data = africa_countries %>% 
                filter(name == input$country_r | 
                         # Handle potential name discrepancies between datasets
                         tolower(name) == tolower(input$country_r) |
                         admin == input$country_r),
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
  
  # Output the regression plot with SPEI on x-axis and viridis colors
  output$regression_plot <- renderPlot({
    data <- reactive_data()
    
    if(nrow(data) < 2) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Insufficient data to display plot") +
               theme_void())
    }
    
    # Access SPEI variable by name
    spei_var_name <- input$spei_var
    
    # Create year column as factor for proper ordering
    data$year_factor <- as.factor(data$year)
    
    # Create a plot showing conflict events vs SPEI with viridis color palette
    ggplot(data, aes_string(x = spei_var_name, y = "conflict_events")) +
      geom_point(aes(color = year_factor), size = 3, alpha = 0.8) +
      geom_smooth(method = "glm", method.args = list(family = "poisson"), 
                  color = "black", se = TRUE) +
      scale_color_viridis_d(option = "viridis") +  # Viridis discrete color scale
      labs(
        title = paste("Conflict Events vs SPEI in", input$country_r),
        subtitle = paste("SPEI", gsub("spei_|_month", "", input$spei_var), "Month"),
        x = "SPEI Value",
        y = "Conflict Events",
        color = "Year"
      ) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12)
      )
  })
  
}

shinyApp(ui, server)