library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(readxl)
library(leaflet)
library(httr)
library(jsonlite)
library(sf)
library(stringr)
library(tidyr)

ui <- dashboardPage(
  dashboardHeader(title = span(tagList(icon("train"), "MRT Risk Analysis Dashboard"))),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "welcome", icon = icon("home")),
      menuItem("Vulnerability Map", tabName = "map", icon = icon("exclamation-triangle")),
      menuItem("Connectivity Map", tabName = "map2", icon = icon("project-diagram")),
      menuItem("Risk Analysis", tabName = "analysis", icon = icon("chart-bar"))
      # ,menuItem("Important Predictors", tabname = "predictors", icon = icon("key"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .welcome-content { padding: 20px; line-height: 1.6; }
        .tab-description { 
          background-color: #f9f9f9;
          border-left: 4px solid #3c8dbc;
          padding: 15px;
          margin-bottom: 20px;
        }
        .highlight-box {
          background-color: #e7f4ff;
          border-radius: 5px;
          padding: 15px;
          margin: 10px 0;
        }
        .tab-description {
        transition: all 0.3s ease;
        cursor: pointer;
        border-left: 4px solid #3c8dbc;
        padding: 15px;
        margin-bottom: 20px;
      }
      
      .tab-description:hover {
        transform: translateX(5px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        border-left: 4px solid #2c3e50;
        background-color: #f1f9ff;
      }
      
      .tab-description h4 {
        color: #3c8dbc;
        transition: color 0.3s ease;
      }
      
      .tab-description:hover h4 {
        color: #2c3e50;
      }
      
      .tab-description .fa {
        transition: transform 0.3s ease;
      }
      
      .tab-description:hover .fa {
        transform: scale(1.1);
      }
      
      /* Link styling */
      .tab-link {
        display: inline-block;
        margin-top: 8px;
        color: #3c8dbc;
        font-weight: bold;
        transition: all 0.2s ease;
      }
      
      .tab-link:hover {
        color: #e74c3c;
        text-decoration: none;
        transform: translateX(3px);
      }
      .analysis-methodology-content {
        padding: 20px;
            }
      .dataTables_length {
        float: left !important;
        margin-right: 20px;
      }
      .dataTables_filter {
        float: right !important;
      }
  
      .methodology-card {
        background: white;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 20px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        height: 100%;
      }
      
      .model-details {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin: 10px 0;
      }
      
      .combined-risk-section {
        background-color: #f1f8ff;
        padding: 20px;
        border-radius: 8px;
        margin-top: 20px;
      }
      
      .box .fa {
        margin-right: 8px;
      }
      
      @media (max-width: 768px) {
        .methodology-card {
          margin-bottom: 15px;
        }
      }
      ")),
      tags$script(HTML("
      $(document).on('click', '#vulnerability_desc', function() {
        Shiny.setInputValue('nav_to', 'map');
      });
      $(document).on('click', '#connectivity_desc', function() {
        Shiny.setInputValue('nav_to', 'map2');
      });
      $(document).on('click', '#analysis_desc', function() {
        Shiny.setInputValue('nav_to', 'analysis');
      });
      "))
    ),
    
    tabItems(
      # Welcome Tab
      tabItem(
        tabName = "welcome",
        div(class = "welcome-content",
            h2("Welcome!"),
            div(class = "highlight-box",
                p("This dashboard helps visualize and analyze MRT station vulnerability and connectivity in Singapore"),
                tags$ul(
                  tags$li("Visualize station risk factors"),
                  tags$li("Identify connectivity patterns"),
                  tags$li("Interactive exploration tools")
                ),
                tags$div(class = "external-link-button",
                         tags$a(href = "https://github.com/gotoshleep/DSE3101-choochootrain-.git",
                                target = "_blank",
                                class = "btn btn-primary",
                                "Go to our Github Website"))
            ),
            
            h3("Dashboard Sections"),
            
            div(class = "tab-description", id = "vulnerability_desc",
                h4(icon("exclamation-triangle"), " Vulnerability Map"),
                p("View stations based on risk of breakdowns.")
            ),
            
            div(class = "tab-description", id = "connectivity_desc",
                h4(icon("project-diagram"), " Connectivity Map"),
                p("Explore how well-connected each station is.")
            ),
            
            div(class = "tab-description", id = "analysis_desc",
                h4(icon("chart-bar"), " Risk Analysis"),
                p("Get a quick overview of key stations needing attention.")
            )
            # ,
            # 
            # div(class = "tab-description",
            #     h4(icon("key"), "Important Predictors"),
            #     p("Find out more about key variables")
            # )
        )
      ),
    
      tabItem(tabName = "map",
              fluidRow(
                
                box(width = 8, leafletOutput("vulnerability_map",
                                             height = "600px")),
                
                box(width = 4, 
                    title = "Adjust Real-Time Metrics", background = "light-blue",
                    radioButtons("day_of_week", "Weekday/Weekend:",
                                 choiceNames = c("Weekday", "Weekend"),
                                 choiceValues = c("WEEKDAY", "WEEKENDS/HOLIDAY")
                    ),
                    
                    radioButtons(inputId = "peak_bool",
                                 label = "Choose hour type:",
                                 choiceNames = c("Peak Hour", "Non-Peak Hour"),
                                 choiceValues = c(1, 0)
                    ),
                    
                    radioButtons(inputId = "weather_condn",
                                 label = "Choose weather condition:",
                                 choiceNames = c("No Rain", "Moderate Rain", "Heavy Rain"),
                                 choiceValues = c("fair", "moderate", "heavy")
                    ),
                    
                    numericInput(inputId = "top_number", 
                                 label = "Top # stations(max 20):",
                                 value = 5,
                                 min = 0,    # Optional minimum value
                                 max = 20,   # Maximum value enforced by the UI),
                                 step = 1
                    ),
                    
                    actionButton("update", "Apply Metrics", icon = icon("sync"))
                ),
                
                box(width = 4,
                    title = "Top Vulnerable Stations",
                    DTOutput("top_vulnerable_table1"))
              )
      ),
      
      tabItem(tabName = "map2",
              fluidRow(
                
                box(width = 8, leafletOutput("connectivity_map",
                                             height = "600px")),
                
                box(width = 4, 
                    title = "Top # stations(max 20):", background = "light-blue",
                    numericInput(inputId = "top_number_c", 
                                 label = "Top # stations(max 20):",
                                 value = 5,
                                 min = 0,    # Optional minimum value
                                 max = 20,   # Maximum value enforced by the UI),
                                 step = 1
                    ),
                    
                    actionButton("update2", "Apply Metrics", icon = icon("sync"))
                ),
                
                box(width = 4,
                    title = "Bottom Connectivity Stations",
                    DTOutput("low_connectivity_table1"))
              )
      ),
      
      tabItem(
        tabName = "analysis",
        div(class = "analysis-methodology-content",
            
            h2(icon("chart-bar"), " Risk Analysis Tables"),
            fluidRow(
              box(width = 6, 
                  title = "Table of Most Vulnerable Stations",
                  status = "danger",
                  solidHeader = TRUE,
                  DTOutput("top_vulnerable_table"),
                  footer = "Higher scores indicate greater vulnerability risk"
              ),
              box(width = 6,
                  title = "Table of Least Connected Stations", 
                  status = "warning",
                  solidHeader = TRUE,
                  DTOutput("low_connectivity_table"),
                  footer = "Lower scores indicate poorer connectivity"
              )
            ),
            
            h2(icon("book"), " Important Predictors"),
            fluidRow(
              # Vulnerability Methodology
              column(width = 6,
                     div(class = "methodology-card",
                         h3(icon("exclamation-triangle"), " Vulnerability Scoring"),
                         div(class = "model-details",
                             h4("XGBoost Key Variables:"),
                             tags$ul(
                               tags$li(tags$strong("Line Age"), "- Age of the station. Older stations may require more infrastructure maintenance."),
                               tags$li(tags$strong("Peakhour"), " - Peak hours of 7am-9am and 6pm-8pm. Peak hours generally see higher passenger volume and shortened train interval timings. This may place more stress on the system."),
                               tags$li(tags$strong("Rainfall(mm)"), " - The numeric average (in mm) of rainfall based on the weather condition. Moderate Rain maps to the median amount of rainfall at the station in the event of rain, while Heavy Rain maps to the maximum amount."),
                               tags$li(tags$strong("Average Riders"), " - Average passenger volume at peak and non-peak hours. Higher passenger volume places more stress on transport infrastucture.")
                             ),
                             h4("Categories:"),
                             tags$ul(
                               tags$li(tags$strong("Day"), " - Is it the weekday or the weekend"),
                               tags$li(tags$strong("Peakhour"), " - Peak hours of 7am-9am and 6pm-8pm. Peak hours generally see higher passenger volume and shortened train interval timings. This may place more stress on the system."),
                               tags$li(tags$strong("Weather Condition"), " - Weather condition at the station.")
                             )
                         )
                     )
              ),
              
              # Connectivity Methodology
              column(width = 6,
                     div(class = "methodology-card",
                         h3(icon("project-diagram"), " Connectivity Scoring"),
                         div(class = "model-details",
                             h4("Key Variables:"),
                             tags$ul(
                               tags$li(tags$strong("MRT Score"), " - 40% weight"),
                               tags$li(tags$strong("Walk Score"), " - 25% weight"),
                               tags$li(tags$strong("Bus score"), " - 35% weight"),
                             )
                         ),
                         h4("Variable Breakdown:"),
                         tags$ul(
                           tags$li("MRT Score: The number of altenative mrt lines at a station"),
                           tags$li("Walk Score: Measures how accessible other MRT stations are by walking",
                                   tags$ul(
                                     tags$li("The walking path must take ≤15 min to complete")
                                   )
                                   ),
                           tags$li("Bus Score: Measures how accessible other MRT stations are by bus",
                                   tags$ul(
                                     tags$li("Bus services must be within 500m of the mrt station"),
                                     tags$li("Bus routes must connect to alternate stations and must be ≤ 2km")
                                   ))
                         )
                     )
              )
            )
        )
      )
  )
)
)


############################################read in and manipulate data for use################################################
df <- st_read("sg-rail.geo.json")
mrtline_df <- df %>% st_zm(drop = T, what = "ZM") %>% 
  filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING")) %>%
  filter(grepl("Line", name, ignore.case = TRUE))
#https://github.com/cheeaun/railrouter-sg/blob/master/src/sg-rail.geo.json
#thank you train nerd ily 

v_df <- read.csv("vul_scores_dynamic.csv") %>% rename("avg_score" = "xgb_scaled")
latlng_data <- read_xlsx("MRT_DATA.xlsx") %>% select(-stations)
vul_data<-v_df %>% arrange(stations, day_type,is_peak) %>% ## ensure that it is ordered by station_code, then weekday then peak status 
  mutate(status = paste0(day_type,is_peak)) %>%
  select(station_code, stations, vul_category, status, line_code, weather_condition) %>%
  mutate(vul_category = case_when(
    vul_category == "Very Low" ~ "<span style=\"color:#094A25;\"><b>Very Low</b></span>",
    vul_category == "Low" ~ "<span style=\"color:#0C6B37;\"><b>Low</b></span>",
    vul_category == "Medium" ~ "<span style=\"color:#F8B324;\"><b>Medium</b></span>",
    vul_category == "High" ~ "<span style=\"color:#EB442C;\"><b>High</b></span>",
    vul_category == "Very High" ~ "<span style=\"color:#BC2023;\"><b>Very High</b></span>"
  )) %>%
  mutate(colour = case_when(
    line_code == "CCL" ~ "orange",
    line_code == "TEL" ~ "saddlebrown",
    line_code == "DTL" ~ "darkslateblue",
    line_code == "EWL" ~ "mediumseagreen",
    line_code == "NEL" ~ "darkmagenta",
    line_code == "NSL" ~ "orangered"
  )) %>% ##giving colour to each line
  pivot_wider(names_from = status, values_from = vul_category) %>% #to access all info in each row
  mutate(information = paste0("<h5 style='margin-bottom:2px'><b><span style=\"color:white; background-color:",colour,";border-radius: 8px; padding: 1px 4px;\">",station_code,"</span> ", stations,"</b></h5>", 
#                             "Vulnerability quantiles:", 
                             "Weekday Offpeak: ", WEEKDAY0, 
                             "<br>Weekday Peak: ", WEEKDAY1, 
                             "<br>Weekend/Holiday Offpeak: ", `WEEKEND/HOLIDAY0`,
                             "<br>Weekend/Holiday Peak: ", `WEEKEND/HOLIDAY1`

  )) %>% #create data for the pop up
  left_join(y = latlng_data, by = "station_code") %>% #add lat long data to vulnerability data 
  select(-WEEKDAY0, -WEEKDAY1, -`WEEKEND/HOLIDAY0`, -`WEEKEND/HOLIDAY1` ) %>%
  arrange(stations) %>%
  group_by(stations, weather_condition) %>%
  mutate(stationcode_w_colour = 
           paste0("<span style=\"color:white; background-color:",colour,";border-radius: 8px; padding: 1px 4px;\">",
                  station_code,"</span> ")) %>% ##surround station codes with html that changes its colour and has borders 
  summarise(
    across(where(is.numeric), ~mean(., na.rm = TRUE)),
    across(station_code, ~paste(., collapse = "/")),
    across(information, ~paste(., collapse = "<br>")),
    across(stationcode_w_colour, ~paste(., collapse = " "))
  ) %>% ##combine interchanges
  mutate(station_w_code = paste(stationcode_w_colour, stations))

v_df <- left_join(x=v_df, y = latlng_data, by = "station_code") #add lat long data to vulnerability data
input <- data.frame(day_of_week = c("WEEKDAY"), peak_bool = c(1), weather_condn = c("fair"))

###initialise the top 5 most vulnerable stations. 
top_vul <- v_df %>% arrange(desc(avg_score)) %>% 
  filter(day_type == input$day_of_week &
           is_peak == input$peak_bool &
           weather_condition == input$weather_condn) %>% head(5)
##############################################################################################################################

####################################Connectivity map####################################
add_data_c <- read.csv("final_score.csv") %>%
  select(-line_code, -line_number, -line) %>%
  group_by(stations)%>%
  summarise(
    across(station_code, ~paste(., collapse = "/")),
    across(-station_code, ~first(.)),
  ) %>%
  mutate(Reachable_Stations = gsub("MRT STATION", "", Reachable_Stations)) %>% #remove the string "MRT STATION"
  mutate(Reachable_Stations = gsub("\\(.*?\\)", "", Reachable_Stations)) %>% #remove everything in paranthesis, including the paranthesis
  mutate(Reachable_Stations = lapply(lapply(strsplit(Reachable_Stations, "/"),trimws),unique)) %>% #convert to list. remove all white space, and keep the unique results only. have to convert to list to use unique
  mutate(Reachable_Stations = sapply(Reachable_Stations, paste, collapse = "<br>- ")) %>% #add a dash and a break behind each station for readability purposes
  mutate(Reachable_Stations = str_to_title(tolower(Reachable_Stations))) %>% #make entries more readable for humans. to be used in the pop up later 
  select(stations, Reachable_Stations, join_station) #to prevent issues when joining later, only take relevant cols


c_df <- read.csv("score_final.csv") %>% mutate(Score=score) %>% select(-score)

connect_data<-c_df %>%
  select(station_code, stations, Score, line_code, mrt_score, bus_score, walk_score) %>%
  inner_join(y = latlng_data, by = "station_code") %>% #add lat long data to vulnerability data
  inner_join(y= add_data_c, by = "stations") %>%
  mutate(colour = case_when(
    line_code == "CCL" ~ "orange",
    line_code == "TEL" ~ "saddlebrown",
    line_code == "DTL" ~ "darkslateblue",
    line_code == "EWL" ~ "mediumseagreen",
    line_code == "NEL" ~ "darkmagenta",
    line_code == "NSL" ~ "orangered"
  )) %>% ##giving colour to each line
  mutate(Score = round(Score,3)) %>% ## separated from previous for clarity
  group_by(stations) %>% 
  mutate(stationcode_w_colour = 
           paste0("<span style=\"color:white; background-color:",colour,";border-radius: 8px; padding: 1px 4px;\">",
                  station_code,"</span> ")) %>%
  summarize(
    Score = mean(Score),
    across(station_code, ~paste(., collapse = "/")),
    across(stationcode_w_colour, ~paste(., collapse = " ")),
    across(c(latitude, longitude, mrt_score, bus_score, walk_score), ~mean(.)),
    across(c(colour, Reachable_Stations), ~first(.))
    )%>% 
  ungroup() %>% 
  mutate(Score = as.numeric(Score)*5) %>%
  mutate(quantile = cut(Score, breaks = quantile(Score, probs = seq(0, 1, 1/3), na.rm = TRUE),
                        labels = c("<span style=\"color:#cd2626;\">Low Connectivity</span>", "<span style=\"color:goldenrod;\">Average Connectivity</span>", "<span style=\"color:forestgreen;\">High Connectivity</span>"), 
                        include.lowest = TRUE))%>%
  mutate(station_w_code = paste(stationcode_w_colour, stations)) %>%
  mutate(information = paste0("<h5 style='margin-bottom:2px'><b>",stationcode_w_colour, stations,"</b></h5>",
                              "<b>", quantile,
                             "</b><br>Connectivity Score: ", Score, 
                             "<br>Reachable Stations: <br>- ", Reachable_Stations
  ))#create data for the pop up


c_df <- connect_data%>% select(station_code, stations, latitude, longitude, mrt_score, bus_score, walk_score, Score)


###initialise the bot 5 least connected stations. 
bot5_connect <- c_df %>% arrange(Score) %>% head(5)
##############################################################################################################################

least_connected <- c_df %>%
  arrange(Score, stations) %>%
  mutate(Score= round(Score,2))


##initialise with weather condition as fair, or the map will update as soon as you click a button
vulmap_data_init <- vul_data %>% filter(weather_condition == input$weather_condn)

server <- function(input, output, session) {
  
  observeEvent(input$nav_to, {
    updateTabItems(session, "tabs", input$nav_to)
  })
  
  output$vulnerability_map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("CartoDB.Voyager", options = providerTileOptions(minZoom = 10.5, maxZoom = 13)) %>% 
      #CartoDB.Voyager light mode, Stadia.AlidadeSmoothDark dark mode
      setView(lat = 1.3521, lng = 103.8018, zoom = 10.5) %>% #keep the focus on singapore, do not let user exit it 
      setMaxBounds(lng1 = 103.557,lat1=1.129, lng2 = 104.131, lat2 = 1.6) %>% #do not let user zoom in or zoom out too much 
      addPolylines(data = mrtline_df, 
                   opacity = 1, 
                   color = ~line_color) %>% #add the mrt track lines
      addCircleMarkers(data = vulmap_data_init,
                       lat = ~latitude,
                       lng = ~longitude,
                       label = ~station_w_code %>% lapply(htmltools::HTML), 
                       popup = ~information,
                       fillOpacity = 1,
                       fillColor = ~ifelse(stations %in% top_vul$stations , "red","white"),
                       opacity = 1, 
                       color = "black", 
                       radius = ~ifelse(stations %in% top_vul$stations , 6,4), 
                       weight = 1.5 #controls the width of outer circle
      ) 
  })
  
  output$connectivity_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Voyager", options = providerTileOptions(minZoom = 10.5, maxZoom = 13)) %>% 
      #CartoDB.Voyager light mode, Stadia.AlidadeSmoothDark dark mode
      setView(lat = 1.3521, lng = 103.8018, zoom = 10.5) %>% #keep the focus on singapore, do not let user exit it 
      setMaxBounds(lng1 = 103.557,lat1=1.129, lng2 = 104.131, lat2 = 1.6) %>% #do not let user zoom in or zoom out too much 
      addPolylines(data = mrtline_df, 
                   opacity = 1, 
                   color = ~line_color) %>% #add the mrt track lines
      addCircleMarkers(data = connect_data, 
                       lat = ~latitude, 
                       lng = ~longitude, 
                       label = ~station_w_code %>% lapply(htmltools::HTML), 
                       popup = ~information,
                       fillOpacity = 1,
                       fillColor = ~ifelse(stations %in% bot5_connect$stations , "red","white"),
                       opacity = 1, 
                       color = "black", 
                       radius = ~ifelse(stations %in% bot5_connect$stations, 6,4), 
                       weight = 1.5 #controls the width of outer circle
      ) 
  })
  
  observeEvent(input$connectivity_map_marker_click, {
    click <- input$connectivity_map_marker_click
    req(click$id)
    
    station_code <- gsub("^all_|^top_", "", click$id)
    
    # station <- connectivity_data_map %>%
    #   filter(station_code == !!station_code)
    
    req(nrow(station) > 0)
    
    showModal(modalDialog(
      title = paste("Station:", station$station),
      tagList(
        h4(paste("Connectivity Score:", station$Score)),
        h4(ifelse(grepl("^top_", click$id), 
                  "Top 5 Least Connected", 
                  ""))
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  ################################################################################
  ## Table(reactive) for TAB1 (vulnerability map)
  ################################################################################
  vulnerable_reactive_table <- eventReactive(input$update, {
    req(input$day_of_week, input$peak_bool, input$weather_condn)
    
    vul_data <- v_df %>%
      filter(day_type == input$day_of_week, 
             is_peak == input$peak_bool,
             weather_condition == input$weather_condn) %>%
      arrange(desc(avg_score), stations) %>%
      head(input$top_number) %>%
      select(station_code, stations, avg_score) %>%
      mutate(avg_score = round(avg_score, 2))
    
    ridership <- read.csv("ridership_by_stations_6months.csv") %>%
      group_by(DAY_TYPE, is_peak, stations)%>%
      summarise(AVG_RIDERS = mean(AVG_RIDERS)) %>%
      ungroup()
    
    rider_vul_data <- ridership %>%
      filter(DAY_TYPE == input$day_of_week, 
             is_peak == input$peak_bool) %>%
      select(stations, AVG_RIDERS) %>%
      mutate(AVG_RIDERS = round(AVG_RIDERS))
    
    left_join(vul_data, rider_vul_data, by = "stations") %>%
      rename("Station Code" = station_code,
             "Station" = stations,
             "Vulnerability Score" = avg_score,
             "Avg Riders" = AVG_RIDERS) %>%
      arrange(desc(`Vulnerability Score`)) %>%
      unique()
  }, ignoreNULL = FALSE)
  
  output$top_vulnerable_table1 <- renderDT({
    vulnerable_reactive_table() %>%
      datatable(
        options = list(
          dom = 'tp', 
          pageLength = 5,
          autoWidth = TRUE,
          scrollX = TRUE, 
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE
      )
  })
  ################################################################################
  ## Table(reactive) for TAB2 (connectivity map)
  ################################################################################
  
  connectivity_reactive_table <- eventReactive(input$update2, {
    
    con_data <- c_df %>%
      arrange(Score, stations) %>%
      head(input$top_number_c) %>%
      select(station_code, stations, Score) %>%
      mutate(Score = round(Score, 2)) %>%
      rename("Station Code" = station_code,
             "Station" = stations,
             "Connectivity Score" = Score)
    
    rider_con_data <- read.csv("ridership_by_stations.csv") %>%
      select(stations, AVG_RIDERS) %>%
      mutate(AVG_RIDERS = round(AVG_RIDERS))
    
    left_join(con_data, rider_con_data, by = c("Station" = "stations")) %>%
      group_by(`Station Code`, Station, `Connectivity Score`) %>%
      summarize(`Avg Riders` = round(mean(AVG_RIDERS, na.rm = TRUE)),
                .groups = "drop") %>%
      arrange(`Connectivity Score`) %>%
      unique()
  
  }, ignoreNULL = FALSE)
  
  output$low_connectivity_table1 <- renderDT({
    connectivity_reactive_table() %>%
      datatable(
        options = list(
          dom = 'tp', 
          pageLength = 5,
          autoWidth = TRUE,
          scrollX = TRUE, 
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE
      )
  })
  
  ################################################################################
  ## Table(top5 stations) for TAB3
  ################################################################################
  # Top Vulnerable stations overall
  ridership <- read.csv("ridership_by_stations_6months.csv") %>%
    group_by(DAY_TYPE, is_peak, stations)%>%
    summarise(AVG_RIDERS = mean(AVG_RIDERS)) %>%
    ungroup() %>%
    mutate(AVG_RIDERS = round(AVG_RIDERS))
  
  most_vulnerable <- v_df %>%
    na.omit() %>% 
    arrange(desc(avg_score), stations) %>%
    mutate(avg_score = round(avg_score,2)) %>%
    left_join(ridership, by= c("day_type"= "DAY_TYPE", "is_peak", "stations")) %>%
    select(stations, station_code,line_age, day_type, is_peak, rain_fall.mm., weather_condition, AVG_RIDERS ,avg_score, vul_category) 
    
  
  output$top_vulnerable_table <- renderDT({
    data = most_vulnerable %>%
      mutate(weather_condition = case_when(
        weather_condition == "fair" ~ "No Rain",
        weather_condition == "moderate" ~ "Moderate Rain",
        weather_condition == "heavy" ~ "Heavy Rain"
      ),
      is_peak = case_when(
        is_peak == 1 ~ "Yes",
        is_peak == 0 ~ "No"
      ),
      rain_fall.mm. = round(rain_fall.mm., 3),
      day_type = str_to_title(day_type)
      ) %>%
      rename("Station" = stations,
             "Station Code" = station_code,
             "Line Age" = line_age,
             "Day" = day_type,
             "Peakhour" = is_peak,
             "Rainfall(mm)" = rain_fall.mm.,
             "Weather Condition" = weather_condition,
             "Average Riders" = AVG_RIDERS,
             "Vulnerability Score" = avg_score,
             "Vulnerability Quantile" = vul_category
             )
    datatable(data,
              options = list(
                dom = 'tip',  # 't' for table, 'i' for information, 'p' for pagination
                pageLength = 5,
                autoWidth = TRUE,
                scrollX = TRUE, 
                lengthMenu = c(5, 10, 15, 20),  # Optional: allows users to change page length
                pagingType = "numbers",
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")  # Center all columns
                )# Shows page numbers instead of simple next/previous,
              ))
  })
  
  
  #Top Least Connected Stations
  output$low_connectivity_table <- renderDT({
    data = least_connected %>%
      mutate(walk_score = round(walk_score, 2), bus_score = round(bus_score,2)) %>%
      mutate(across(c(walk_score,bus_score), ~replace_na(.,0))) %>%
      select(station_code, stations, Score, mrt_score, walk_score, bus_score) %>%
      rename("Station" = stations,
             "Station Code" = station_code,
             "MRT Score" = mrt_score,
             "Walk Score" = walk_score, 
             "Bus Score" = bus_score
             )
    datatable(data,
              options = list(
                dom = 'tip',  # 't' for table, 'i' for information, 'p' for pagination
                pageLength = 5,
                autoWidth = TRUE,
                scrollX = TRUE, 
                lengthMenu = c(5, 10, 15, 20),  # Optional: allows users to change page length
                pagingType = "numbers",  # Shows page numbers instead of simple next/previous
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")  # Center all columns
                )
              ))
  })
  ################################################################################
  ################################################################################
  
  #action button updates the leaflet plot (for vulnerability map)
  observeEvent(input$update, {
    ###when the parameters are input, update the top 5 most vulnerable stations 
    top_vul <- v_df %>% arrange(desc(avg_score)) %>% 
      filter(day_type == input$day_of_week & is_peak == input$peak_bool & weather_condition == input$weather_condn) %>% 
      head(input$top_number)
    vulmap_data <- vul_data %>%filter(weather_condition == input$weather_condn)
    leafletProxy("vulnerability_map") %>%  
      clearMarkers() %>%      # Clear previous markers
      addCircleMarkers(
        data = vulmap_data,
        lat = ~latitude,
        lng = ~longitude,
        label = ~station_w_code %>% lapply(htmltools::HTML),
        popup = ~information,
        fillOpacity = 1,
        fillColor = ~ifelse(stations %in% top_vul$stations, "red", "white"),
        color = "black",
        radius = ~ifelse(stations %in% top_vul$stations, 6, 4),
        weight = 1.5
      ) ##apply new markers
  })
  
  #action button updates the leaflet plot (for connectivity map)
  observeEvent(input$update2, {
    ###when the parameters are input, update the bottom 5 least connected stations 
    bot_con <- c_df %>% arrange(Score) %>% head(input$top_number_c)
    leafletProxy("connectivity_map") %>%  
      clearMarkers() %>%      # Clear previous markers
      addCircleMarkers(data = connect_data, 
                       lat = ~latitude, 
                       lng = ~longitude, 
                       label = ~station_w_code %>% lapply(htmltools::HTML), 
                       popup = ~information,
                       fillOpacity = 1,
                       fillColor = ~ifelse(stations %in% bot_con$stations , "red","white"),
                       opacity = 1, 
                       color = "black", 
                       radius = ~ifelse(stations %in% bot_con$stations, 6,4), 
                       weight = 1.5 #controls the width of outer circle
      )
  })
  
  output$data_table <- renderTable({
    data()  # Display current data
  })
}

shinyApp(ui = ui, server = server)

