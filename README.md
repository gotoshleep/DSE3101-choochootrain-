# DSE3101-choochootrain-
# 🚇 MRT Station Vulnerability & Connectivity Dashboard

This is an interactive R Shiny dashboard that visualizes **Vulnerability** and **Connectivity** scores of MRT stations across Singapore.

This project aims to identify which MRT stations require the most attention when the Land Transport Authority (LTA) seeks to enhance Singapore’s transportation network.

##### 🔗 You can access the interactive dashboard [here](https://3trgty-ng-yun0xuan.shinyapps.io/ChooChooTrain/).
---

### Setup Instructions

1. Install R and RStudio.
2. Clone this repository:
   git clone https://github.com/gotoshleep/DSE3101-choochootrain-.git
3. Open the project folder in RStudio.
4. Install these packages by running the following command in the console:
   install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "DT", "readlxl", "leaflet", "httr", "jsonlite", "sf", "stringr", "tidyr"))

---
  
### Run the App Locally
To launch the dashboard locally:
1. Open RStudio.
2. Set your working directory to the cloned project folder.
3. Open the app file located at:
   frontend/Dashboard v7.R
4. Click Run App, or run the following command in the console:
   shiny::runApp("frontend/Dashboard v7.R")

---

### Descriptions of Folders and Files
  
- **`Raw Data/`**: This folder contains the raw datasets used.
  - `2017-march2025 mrt updates.html`:
  - `LTA MRT Station Exit (GEOJSON).geojson`:
  - `bus_route.json`:
  - `bus_stops.json`:
  - `rainfall_data.zip`:
  - `station_data.json`:
  - `transport_node_train_20232024collated.csv`:

- **`backend/`**: This folder contains the R scripts that handle the processing of raw data and model building.
  - `DSE3101 Project.Rmd`:
  - `bus_score.csv`:
  - `connectivity_score.csv`:
  - `mrt_score.csv`:
  - `mrt_stations`:
  - `score.csv`:
  - `score_final.csv`:
  - `vulnerability_cleaning`:
  - `vulnerability_model`:
  - `walk_score.csv`:

- **`frontend/`**: This folder contains the R Shiny UI and server logic that power the interactive dashboard, along with the necessary files used to build and render the application’s user interface and display the data.
  - `Dashboard v7.R`: This is the main Shiny UI and server file that powers
the interactive dashboard.
  - `MRT_DATA.xlsx`: This file provides the location data (longitude and latitude) for each MRT station.
  - `ridership_by_stations_6months.csv`: This file shows the ridership data for each MRT station over the 6-month period relevant to our analysis.
  - `score_final.csv`: This file contains the connectivity scores.
  - `sg-rail.geo.json`: This file contains geographical data representing the Singapore MRT rail network. This data was sourced from https://github.com/cheeaun/railrouter-sg/blob/master/src/sg-rail.geo.json
  - `vul_scores_dynamic.csv`: This file contains the vulnerability scores.
---
  
### How Our Models Work, Pre-Processing Steps, Cross-Validation Steps
