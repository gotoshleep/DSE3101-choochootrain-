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
                             h4("XGBoost Selected Variables:"),
                             tags$ul(
                               tags$li(tags$strong("riders_per_station"), " - Passenger Volume"),
                               tags$li(tags$strong("average_monthly_rainfall.mm."), " - Weather Impact"),
                               tags$li(tags$strong("Line codes"), " - Infrastructure age/type"),
                               tags$li(tags$strong("is_morning_peak"), " - Time Period")
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
                               tags$li(tags$strong("Walking time (≤15 mins)"), " - 30% weight"),
                               tags$li(tags$strong("Shared bus services"), " - 20% weight"),
                               tags$li(tags$strong("Accessible stations"), " - 15% weight"),
                               tags$li(tags$strong("Interchange status"), " - 35% weight")
                             )
                         ),
                         h4("Connectivity Definition:"),
                         p("Well-connected stations provide:"),
                         tags$ul(
                           tags$li("Bus services within 500m walking distance"),
                           tags$li("≤15 min walking paths to other MRT stations"),
                           tags$li("Bus routes with ≤2km connections to alternate stations")
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

v_df <- read.csv("vul_scores_time.csv")
latlng_data <- read_xlsx("MRT_DATA.xlsx") %>% select(-stations)
vul_data<-v_df %>% arrange(stations, day_type,is_peak) %>% ## ensure that it is ordered by station_code, then weekday then peak status 
  mutate(status = paste0(day_type,is_peak)) %>%
#  group_by(stations) %>% mutate(number = n()) %>% ungroup() %>%
  select(station_code, stations, vul_category, status, line_code) %>%
  mutate(colour = case_when(
    line_code == "CCL" ~ "orange",
    line_code == "TEL" ~ "saddlebrown",
    line_code == "DTL" ~ "darkslateblue",
    line_code == "EWL" ~ "mediumseagreen",
    line_code == "NEL" ~ "darkmagenta",
    line_code == "NSL" ~ "orangered"
  )) %>% ##giving colour to each line
  pivot_wider(names_from = status, values_from = vul_category) %>%
  mutate(information = paste0("<h5 style='margin-bottom:2px'><b><span style=\"color:white; background-color:",colour,";border-radius: 8px; padding: 1px 4px;\">",station_code,"</span> ", stations,"</b></h5>", 
#                             "Vulnerability quantiles:", 
                             "Weekday Offpeak: ", WEEKDAY0, 
                             "<br>Weekday Peak: ", WEEKDAY1, 
                             "<br>Weekend/Holiday Offpeak: ", `WEEKENDS/HOLIDAY0`
  )) %>% #create data for the pop up
  left_join(y = latlng_data, by = "station_code") %>% #add lat long data to vulnerability data 
  select(-WEEKDAY0, -WEEKDAY1, -`WEEKENDS/HOLIDAY0` ) %>%
  arrange(stations) %>%
  group_by(stations) %>%
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
input <- data.frame(day_of_week = c("WEEKDAY"), peak_bool = c(1))

###initialise the top 5 most vulnerable stations. 
top_vul <- v_df %>% arrange(desc(avg_score)) %>% filter(day_type == input$day_of_week & is_peak == input$peak_bool) %>% head(5)
##############################################################################################################################

####################################Connectivity map####################################
add_data_c <- read.csv("final_score.csv") %>%
  select(-line_code, -line_number, -join_station, -line) %>%
  group_by(stations)%>%
  summarise(
    across(station_code, ~paste(., collapse = "/")),
    across(-station_code, ~first(.)),
  ) %>%
  mutate(Reachable_Stations = gsub("MRT STATION", "", Reachable_Stations)) %>% #remove the string "MRT STATION"
  mutate(Reachable_Stations = gsub("\\(.*?\\)", "", Reachable_Stations)) %>% #remove everything in paranthesis, including the paranthesis
  mutate(Reachable_Stations = lapply(lapply(strsplit(Reachable_Stations, "/"),trimws),unique)) %>% #convert to list. remove all white space, and keep the unique results only. have to convert to list to use unique
  mutate(Reachable_Stations = sapply(Reachable_Stations, paste, collapse = "<br>- ")) %>%
  mutate(Reachable_Stations = str_to_title(tolower(Reachable_Stations))) %>% 
  select(stations, Reachable_Stations, avg_bus_score, avg_walk_score, avg_mrt_score) #to prevent issues when joining later


c_df <- read.csv("connectivity_score.csv")
connect_data<-c_df %>%
  select(station_code, stations, Score, line_code) %>%
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
    across(c(latitude, longitude), ~mean(.)),
    across(c(colour, Reachable_Stations), ~first(.))
    )%>%
  ungroup() %>%
  mutate(Score = as.numeric(Score)*5) %>%
  mutate(station_w_code = paste(stationcode_w_colour, stations)) %>%
  mutate(information = paste0("<h5 style='margin-bottom:2px'><b>",stationcode_w_colour, stations,"</b></h5>",
                             "Connectivity Score: ", Score, 
                             "<br>Reachable Stations: <br>- ", Reachable_Stations
  ))#create data for the pop up


c_df <- left_join(x=c_df, y = latlng_data, by = "station_code") #add lat long data to vulnerability data
input <- data.frame(day_of_week = c("WEEKDAY"), peak_bool = c(1))


###initialise the bot 5 least connected stations. 
bot5_connect <- c_df %>% arrange(Score) %>% head(5)
##############################################################################################################################

least_connected <- c_df %>%
  arrange(Score) %>%
  mutate(Score= round(Score,2))

most_vulnerable <- v_df %>%
  # na.omit()%>% 
  group_by(stations, station_code) %>% 
  summarize(mean_score = mean(avg_score)) %>%
  arrange(desc(mean_score))%>%
  mutate(mean_score = round(mean_score,2))

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
      addCircleMarkers(data = vul_data,
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
    req(input$day_of_week, input$peak_bool)
    
    vul_data <- v_df %>%
      filter(day_type == input$day_of_week, 
             is_peak == input$peak_bool) %>%
      arrange(desc(avg_score)) %>%
      head(input$top_number) %>%
      select(station_code, stations, avg_score) %>%
      mutate(avg_score = round(avg_score, 2))
    
    rider_vul_data <- read.csv("ridership_by_stations.csv") %>%
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
      arrange(Score) %>%
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
  output$top_vulnerable_table <- renderDT({
    data = most_vulnerable %>%
      select(station_code, stations, mean_score) %>%
      rename("Station" = stations,
             "Station_Code" = station_code)
    datatable(data,
              options = list(
                dom = 'tip',  # 't' for table, 'i' for information, 'p' for pagination
                pageLength = 5,
                lengthMenu = c(5, 10, 15, 20),  # Optional: allows users to change page length
                pagingType = "numbers"  # Shows page numbers instead of simple next/previous
              ))
  })
  
  
  #Top Least Connected Stations
  output$low_connectivity_table <- renderDT({
    data = least_connected %>%
      select(station_code, stations, Score) %>%
      rename("Station" = stations,
             "Station_Code" = station_code)
    datatable(data,
              options = list(
                dom = 'tip',  # 't' for table, 'i' for information, 'p' for pagination
                pageLength = 5,
                lengthMenu = c(5, 10, 15, 20),  # Optional: allows users to change page length
                pagingType = "numbers"  # Shows page numbers instead of simple next/previous
              ))
  })
  ################################################################################
  ################################################################################
  
  #action button updates the leaflet plot (for vulnerability map)
  observeEvent(input$update, {
    ###when the parameters are input, update the top 5 most vulnerable stations 
    top_vul <- v_df %>% arrange(desc(avg_score)) %>% filter(day_type == input$day_of_week & is_peak == input$peak_bool) %>% head(input$top_number)
    leafletProxy("vulnerability_map") %>%  
      clearMarkers() %>%      # Clear previous markers
      addCircleMarkers(
        data = vul_data,
        lat = ~latitude,
        lng = ~longitude,
        label = ~station_w_code %>% lapply(htmltools::HTML),
        popup = ~information,
        fillOpacity = 1,
        fillColor = ~ifelse(stations %in% top_vul$stations, "red", "white"),
        color = "black",
        radius = ~ifelse(stations %in% top_vul$stations, 6, 4),
        weight = 1.5
      )
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
