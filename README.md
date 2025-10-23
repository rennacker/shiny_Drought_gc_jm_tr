# Drought and Conflict in Sub-Saharan Africa

An interactive Shiny web application for exploring the relationship between drought conditions and conflict events across the Sahel region of Sub-Saharan Africa.

## Overview

This project provides an interactive platform to analyze patterns of armed conflict in relation to climate conditions across the African continent. Using data from the Armed Conflict Location & Event Data Project (ACLED) and climate indicators, users can explore temporal and spatial relationships between drought conditions and conflict events from 2000-2025.

## Features

- **Interactive Conflict Mapping**: Visualize conflict locations on an interactive map with fatality counts
- **Dynamic Filtering**: Filter by country, year range, and event type
- **Data Tables**: Browse detailed conflict event data with sortable columns
- **Climate Trends**: Visualize drought conditions using SPEI (Standardized Precipitation-Evapotranspiration Index)
- **Spatial Analysis**: Explore geographic distribution of different conflict types

## Data Sources

- **ACLED (Armed Conflict Location & Event Data Project)**: Comprehensive conflict event data including battles, protests, riots, and violence against civilians
- **SPEI**: Standardized Precipitation-Evapotranspiration Index for drought monitoring
- **FAO GAUL**: Global Administrative Unit Layers for geographic boundaries

## Installation

### Prerequisites

- R (version 4.0 or higher recommended)
- RStudio (optional but recommended)
- Git LFS for large data files

### Required R Packages

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "here",
  "readxl",
  "lubridate",
  "DT",
  "leaflet",
  "bslib",
  "gee"
))
```

### Clone the Repository

```bash
# Install Git LFS if you haven't already
git lfs install

# Clone the repository
git clone https://github.com/rennacker/shiny_Drought_gc_jm_tr.git
cd shiny_Drought_gc_jm_tr
```

Note: This repository uses Git LFS for large data files. Ensure Git LFS is installed before cloning.

## Usage

### Running the Application

1. Open RStudio and navigate to the project directory
2. Open `app.R`
3. Click "Run App" in RStudio, or run:

```r
shiny::runApp()
```

### Using the Interface

1. **Select a Country**: Choose from Sub-Saharan African countries in the dropdown menu
2. **Set Year Range**: Use the slider to specify the time period of interest (2000-2025)
3. **Filter Event Types**: Select specific conflict event types to analyze
4. **Click Update**: Refresh visualizations with your selected parameters
5. **Explore Tabs**:
   - **Overview**: Project introduction and background
   - **Conflict Map**: Interactive map of conflict events
   - **Data Summary**: Detailed event data table
   - **Climate Trends**: Drought condition visualizations
   - **Advanced Analysis**: Spatial distribution analysis

## Project Structure

```
shiny_Drought_gc_jm_tr/
├── app.R                          # Main Shiny application
├── README.md                      # Project documentation
├── shiny_Drought_gc_jm_tr.Rproj  # RStudio project file
├── .gitignore                     # Git ignore patterns
├── African_Drought_and_Conflict/  # Additional analysis scripts
└── extra_scripts/                 # Supplementary scripts
```

## Data Files

The following data files are managed via Git LFS:
- `ACLED_Africa_Regions_1-1-1900--1-30-2025.csv` (236 MB)

Documentation files:
- `ACLED_Codebook-2024-7-Oct.-2024.pdf`

## Development

This project was developed as part of ESM 244 (Environmental Science & Management) coursework, focusing on the intersection of climate change and conflict dynamics in Africa.

### Contributors

- Garrett Craig (gc)
- Jackson Mills (jm)
- Taylor Rennacker (tr)

## Citation

If using this application or code, please cite the original data sources:

**ACLED:**
> Raleigh, C., Linke, A., Hegre, H., & Karlsen, J. (2010). Introducing ACLED: An Armed Conflict Location and Event Dataset. Journal of Peace Research, 47(5), 651-660.

## License

Please refer to the original data source licenses for ACLED dataset.

## Acknowledgments

- ACLED Project for conflict event data
- ESM 244 course instructors and teaching staff

## Contact

For questions or issues, please open an issue on the GitHub repository.
