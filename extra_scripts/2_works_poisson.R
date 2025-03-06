library(shiny)
library(tidyverse)
library(fixest)  # for fixed effects Poisson regression
library(ggplot2)  # for creating plots
library(DT)  # for interactive tables
library(viridis)  # for viridis color palette
library(here)
library(broom)  # for tidy model output
library(gt)  # for nice tables
library(sf)  # for spatial data handling
library(rnaturalearth)

# Load the dataset
sahel_pop_with_fill <- read_csv(here("data", "Sahel_Pop_Con_SPEI_Regression_NA.csv"))

# Get Africa country boundaries for the small map
africa_countries <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Define the UI for the app
ui <- fluidPage(
  titlePanel("Poisson Regression of Conflicts"),
  
  sidebarLayout(
    sidebarPanel(
      # Text output to display total population at the top
      tags$div(
        style = "margin-bottom: 15px; font-weight: bold; padding-top: 5px;",
        textOutput("total_population")
      ),
      
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
    ),
    
    mainPanel(
      # Top row with model results and Africa map
      fluidRow(
        column(8, h4("Poisson Regression Model Results"),
               div(style = "padding-right: 0px;", # Reduce right padding
                   gt_output("model_table")
               )
        ),
        # Right column for Africa map
        column(4,
               h4("Location"),
               div(style = "height: 200px; padding-left: 0px;", # Reduce left padding
                   plotOutput("africa_map", height = "100%", width = "100%")
               )
        )
      ),
      
      # Plot to show the relationship between conflict and SPEI
      h4("Conflict Events vs SPEI"),
      plotOutput("regression_plot", height = "400px")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression for filtered data based on selected year range and country
  reactive_data <- reactive({
    data <- sahel_pop_with_fill %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
      filter(country == input$country_r) %>%  # Fixed: using country_r instead of country
      # Select columns
      select(year, country, conflict_events, population, all_of(input$spei_var)) %>%
      drop_na()  # Remove NA rows for the selected columns
    
    return(data)
  })
  
  # Calculate and display total population for selected years and country
  output$total_population <- renderText({
    data <- reactive_data()
    
    if(nrow(data) == 0) {
      return("Total Population: No data available")
    }
    
    # Get the latest population value for each year (in case there are multiple entries per year)
    total_pop <- data %>%
      group_by(year) %>%
      summarize(yearly_pop = last(population)) %>%
      summarize(total = sum(yearly_pop)) %>%
      pull(total)
    
    # Format with commas
    formatted_pop <- format(total_pop, big.mark = ",", scientific = FALSE)
    return(paste("Total Population:", formatted_pop))
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
    
    # Perform the Poisson regression using the feglm function
    tryCatch({
      model <- feglm(formula_obj, family = "poisson", data = data)
      return(model)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Output the model as a clean gt table using broom's tidy function
  output$model_table <- render_gt({
    model <- regression_model()
    
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
          term == "population" ~ "Population",
          TRUE ~ term
        ))
      
      # Create gt table
      tidy_model %>%
        select(term, estimate, std.error, p.value) %>%
        gt() %>%
        tab_header(title = "Poisson Regression Results") %>%
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
        ) %>%
        tab_footnote(
          footnote = "Bold p-values are significant at p < 0.05",
          locations = cells_column_labels(columns = p.value)
        )
    }
  })
  
  # Output the Africa map with selected country highlighted
  # Fixed: renamed from africaLMap to africa_map to match UI definition
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
        title = paste("Conflict Events vs SPEI in", input$country_r),  # Fixed: using country_r
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

# Run the application
shinyApp(ui = ui, server = server)