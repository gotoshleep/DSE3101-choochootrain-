# DSE3101-choochootrain-
# 🚇 MRT Station Vulnerability & Connectivity Dashboard

This is an interactive R Shiny dashboard that visualizes **Vulnerability** and **Connectivity** scores of MRT stations across Singapore.

This project aims to identify which MRT stations require the most attention when the Land Transport Authority (LTA) seeks to enhance Singapore’s transportation network.

##### 🔗 You can access the interactive dashboard [here](https://3trgty-ng-yun0xuan.shinyapps.io/ChooChooTrain/).
---

### Setup Instructions

1. Install R and RStudio, Python.
2. Clone this repository:
   git clone https://github.com/gotoshleep/DSE3101-choochootrain-.git
3. Open the project folder in RStudio.
4. Install these packages by running the following command in the console:
   install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "DT", "readlxl", "leaflet", "httr", "jsonlite", "sf", "stringr", "tidyr", "tidyverse", "rvest", "stringr", "jsonlite", "geosphere", "lubridate","tidytext","chron","future.apply","progressr","xml2","writexl","xgboost","Matrix","caret","randomForest","e1071"))
5. To run `onemapapi data pull.IPYNB` , register an account with OneMap API: https://www.onemap.gov.sg/apidocs/register and enter your email and password in the code to generate your API key
---
  
### Run the App Locally
To launch the dashboard locally:
1. Open RStudio.
2. Set your working directory to the cloned project folder.
3. Open the app file located at:
   frontend/Dashboard v7.R
4. Click Run App, or run the following command in the console:
   `library(shiny); runApp('Dashboard v7.R')`

---

### Descriptions of Folders and Files
  

- **`Backend/`**: This folder contains the R scripts that handle the processing of raw data and model building.
  - `vul_model_dynamic.R`: used for modelling vulnerability score for the dynamic model
  - `breakdown cleaning.RMD`: used for cleaning telegram breakdown messages and produce df referenced in vul_model_static.R
  - `mrt_final.RMD`: used to generate mrt masterlist, rainfall and ridership
  - `vul_model_static.R`: used for modeeling vulnerability score for static model
  - `mrt_final_6_months.RMD`:  used to generate mrt masterlist, rainfall and ridership in 6 months
  - `breakdown cleaning 6 months.RMD`: used to produce df referenced in vul_model_dynamic.R
  - `connectivity.RMD`: used to produce df for connectivity scores
  - `onemapapi data pull.IPYNB`: MRT, bus stop coordinates and MRT to MRT walking path
  - `rainfall data.IPYNB`: Rainfall data pull
  - `raw_data`: contains all the data referenced for backend, also used to hold files that is sent to FE for referencing

- **`frontend/`**: This folder contains the R Shiny UI and server logic that power the interactive dashboard, along with the necessary files used to build and render the application’s user interface and display the data.
  - `Dashboard v7.R`: This is the main Shiny UI and server file that powers the interactive dashboard.
  - `MRT_DATA.xlsx`: This file provides the location data (longitude and latitude) for each MRT station.
  - `ridership_by_stations_6months.csv`: This file shows the ridership data for each MRT station over the 6-month period relevant to our analysis.
  - `score_final.csv`: This file contains the connectivity scores.
  - `sg-rail.geo.json`: This file contains geographical data representing the Singapore MRT rail network. This data was sourced from https://github.com/cheeaun/railrouter-sg/blob/master/src/sg-rail.geo.json
  - `vul_scores_dynamic.csv`: This file contains the vulnerability scores.

---

### Data Sources
The collection of raw data involved compiling various datasets from different sources.
1. [Wikipedia](https://en.wikipedia.org/wiki/Mass_Rapid_Transit_(Singapore)): Train Information: etc: train age, rail cost, rail length
2. [LTA Datamall](): Ridership Data
3. [Data.gov](https://data.gov.sg/): Daily Rainfall
4. [OneMap API](): Bus Routes, Walking Routes
5. [PropertyReview](https://propertyreviewsg.com/complete-singapore-mrt-list-english-and-chinese-station-names/): List of MRT stations with station code and readable name

### Pre-Processing 
#### MRT Stations Masterlist
From the PropertyReview website, we filtered out any stations that have not been constructed yet, added MRT stations that were left out and filtered out LRT stations.
After filtering and cleaning our dataset, we are left with 143 unique MRT stations. The **stations** dataframe itself contains 171 rows of data, as it counts interchange stations as a separate observation with a different line and station code. 
The station code, station name and line name were directly extracted from the Property Review website. These variables have been renamed to station_code, stations and line.
Additional variables that have been added to the data include:
- line_code: which can be extracted from the first two alpha characters in station_code and adding an "L" (standing for "Line") at the end
- line_number: which can be extracted from the numeric digits in station_code
- join_station: a standardised `station_name` variable that can be used to join with other datasets
- is_interchange: An indicator variable with values 0 (not an interchange) and 1 (is an interchange), which is obtained by counting the number of occurrences of a station.
- is_above_ground: An indicator variable with 1 indicating a station is above ground.

Our team also found additional information on station and line characteristics from Wikipedia. The variables extracted from the web scrape are as follows:
- operator: A character variable recording the MRT line's operator; SMRT Trains or SBS Transit
- commencement: The start date of operation for the MRT line.
- line_age: A numeric variable calculated as the difference between today's date and the commencement date.
- n_stations: The number of stations along an MRT line.
- length: The length (in kilometers) of the MRT line.
- cost: The total cost of construction for the MRT line.
- n_lines: the number of lines services by the station
As the Wikipedia page does not contain information on the Changi Airport Branch Line, we have manually added their information.

#### Ridership Data
- Sourced from **LTA DataMall's Passenger Volume by Train Stations**.
- Hourly tap-in/out volumes (Dec 2023-Feb 2024, Dec 2024-Feb 2025)
- Peak periods identified (6-9am, 4-8pm)
- Interchange station volumes proportionally distributed

#### Rainfall Data
- Data was pulled from Data.gov.sg
- 5-minute interval data mapped to nearest MRT stations
- Harvensine distance used for weather station allocation

#### Breakdown Notices
-  Station service disruption notices were scraped from SMRT's official X account (@SMRT_Singapore) via a Telegram bot called SG MRT UPDATES.
-  Natural Language Processing techniques were used to clean the data. Bag of words was used to tokenise messages into a machine-readable format, before funnelling it through Bing sentiment analysis to sort for relevant breakdown messages. Fuzzy matching was used to dissect each message to identify all stations affected by a breakdown incident.

#### Bus and MRT Stop Coordinate Data
Scraped from OneMap API. The data was cleaned and filtered for 
- bus stops located within a walking path of ≤500m of an MRT station
- walking paths between MRT stations to be accessible within 15 minutes of walking
- bus routes within a 2km journey to another MRT station from any given MRT station.

The above conditions were chosen to ensure that any alternative routes taken do not amount to travel times beyond the stipulated 30-minute delays resulting from service disruptions.

---

### How Our Models Work: Vulnerability Model
The vulnerability metric scores a station based on features that we have identified that could indicate potential involvements in a service disruption. These could include a train fault occurring at the station itself, a signalling fault affecting an entire line, or any other disruption that results in a delay of 30 minutes or more. We then identify the top 5 most vulnerable stations by scoring.

The interface uses an XGBoost model trained on a dynamic dataset covering 6 months worth of data (Dec 2023-Feb 2024, Dec 2024-Feb 2025). The predictors involve time-sensitive variables like hourly ridership volume, daily precipitation levels, alongside static infrastructure-related variables like line operators and cost. These variables were chosen to provide a more holistic overview of a station's vulnerability to breakdowns under specific stressors.

We found that XGBoost performed the best amongst our other models (Linear Regression, Logistic Regression, Naive Bayes and Random Forest), and selected features include line ages, peak or off-peak hours, precipitation levels, line code and ridership volume.

### How Our Models Work: Connectivity Model
The connectivity metric scores a station based on how connected it is to other MRT stations, allowing us to gauge its ability to cope with service disruptions. This includes both direct and indirect methods of alternative travel. Direct methods include direct walking paths to alternative stations and interchange station linkages to other lines. Indirect methods include surrounding bus stops with available services to other stations. Each feature is assigned a weight based on its importance and feasibility, and a connectivity score is calculated for every station. We then extract the bottom 5 least connected stations most in need of support.

We define a well-connected station as one that provides commuters with seamless access to alternative routes, such as bus services, accessible walking paths, and interchanges to other MRT stations.

Our connectivity scoring formula is as follows:

$$
Connectivity \ Score = 0.4 \times (\text{Is} \ \text{Interchange}) \times (n_{\text{lines}} - 1) + 0.25 \times \sum_{1}^{n} \log(15 / \text{Walking Time} + 1) + 0.35 \times \sum_{1}^{k} \log(\text{Number of Bus Services} + 1)
$$


1.
$$ 0.4 \times (\text{Is Interchange}) \times (n_{\text{lines}} - 1) $$

The indicator variable is_interchange, representing whether a station is an interchange, is multiplied by the number of operating lines not affected by the breakdown (n_lines - 1). This is assigned the highest weight of 0.4 as it is a convenient and efficient way for commuters to continue their journey to alternative stations.

2. 
$$ 0.25 \times \sum_{i=1}^{n} \log(15 / \text{Walking Time} + 1) $$

A walkable station has been defined as one reachable within 15 minutes or less of walking time. We take the log fraction of maximum walking time (15 min) over the actual walking time (walking_time) and sum it over the number of walkable stations (n). A log scale has been used to penalise longer walking times. A lower weight of 0.25 has been assigned to this factor to account for individuals with mobility issues who may find such connection methods challenging. As walking time increases, the score increases by less as longer walking durations are less desirable.

3.
$$ 0.35 \times \sum_{j=1}^{k} \log(\text{Number of Bus Services} + 1) $$

The sum of the total number of bus services (n_bus_services) at each bus stop (total number of bus stops = k) surrounding the MRT station. Here, we use a log scale to capture diminishing returns where each additional bus service to another MRT station improves connectivity, but each extra one contributes less than the last. A moderate weight of 0.35 has been assigned to this metric as utilising bus services is not as convenient as a direct transfer, but is more accommodating for individuals with mobility issues.
