# Drought and Armed Conflict in Sub-Saharan Africa

An interactive Shiny web application for exploring the relationship between drought conditions and armed conflict events across Sub-Saharan Africa.

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
- **SPEIbase**: Standardized Precipitation-Evapotranspiration Index for drought monitoring
- **geoBoundaries**: Global database of political administrative boundaries
- **WorldPop**: Open spatial demographic data and research

## Installation

### Prerequisites

- R (version 4.0 or higher recommended)
- RStudio (optional but recommended)

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
# Clone the repository
git clone https://github.com/rennacker/shiny_Drought_gc_jm_tr.git
cd shiny_Drought_gc_jm_tr
```

### Download Data Files

Due to the large size of the data files (236+ MB), they are hosted separately on Google Drive.

**Download the data folder here:** [Google Drive - Data Folder](https://drive.google.com/drive/folders/12IeRMvvZwmva499pvuE_AJxZU3DwddFc?usp=sharing)

After downloading:
1. Extract the `data/` folder
2. Place it in the root directory of the cloned repository
3. Your structure should look like:
   ```
   shiny_Drought_gc_jm_tr/
   ├── app.R
   ├── data/           # <- Downloaded data folder goes here
   │   ├── ACLED_Africa_Regions_1-1-1900--1-30-2025.csv
   │   └── ...
   └── ...
   ```

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

**Data files are available for download via Google Drive:** [Download Data Folder](https://drive.google.com/drive/folders/12IeRMvvZwmva499pvuE_AJxZU3DwddFc?usp=sharing)

The data folder includes:
- `ACLED_Africa_Regions_1-1-1900--1-30-2025.csv` (236 MB) - Conflict event data
- `Annual_SPEI_Africa_1980_2025.csv` - Climate drought indicators
- `district_geometries_africa.csv` - Geographic boundary data
- `ACLED_Codebook-2024-7-Oct.-2024.pdf` - Documentation

Note: The data folder must be downloaded separately and placed in the project root directory before running the application.

## Development

This project was developed as part of ESM 244 (Environmental Science & Management) coursework, focusing on the intersection of climate change and conflict dynamics in Africa.

### Contributors

- Garrett Craig (gc)
- Jennifer Meng (jm)
- Taylor Rennacker (tr)

## Citation

If using this application or code, please cite the original data sources:

Armed Conflict Location & Event Data Project (ACLED). 2024. ACLED Version 2024 Dataset. Retrieved from https://acleddata.com.

Beguería, S., Vicente Serrano, S. M., Reig-Gracia, F., Latorre Garcés, B. (2023). SPEIbase v.2.9 [Dataset]. DIGITAL.CSIC. doi:10.20350/digitalCSIC/15470.

Runfola, D. et al. (2020). geoBoundaries: A global database of political administrative boundaries. PLoS ONE 15(4): e0231866. https://doi.org/10.1371/journal.pone.0231866.

WorldPop. (2025). Open spatial demographic data and research. University of Southampton. https://www.worldpop.org/.

## License

Please refer to the original data source licenses for ACLED, SPEIbase, geoBoundaries, and WorldPop datasets.

## Acknowledgments

- ACLED Project for conflict event data
- DIGITAL.CSIC for SPEIbase climate data
- geoBoundaries team for administrative boundary data
- WorldPop at University of Southampton for demographic data
- ESM 244 course instructors and teaching staff

## Contact

For questions or issues, please open an issue on the GitHub repository.
