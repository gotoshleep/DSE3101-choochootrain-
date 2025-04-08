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
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Vulnerability Map", tabName = "map", icon = icon("exclamation-triangle")),
      menuItem("Connectivity Map", tabName = "map2", icon = icon("project-diagram")),
      menuItem("Risk Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Important Predictors", tabname = "predictors", icon = icon("key"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
      
        .welcome-container {
          min-height: 80vh;
          display: flex;
          justify-content: center;
          align-items: center;
          text-align: center;
          padding: 30px;
        }
        
        .welcome-box {
          background-color: linear-gradient(135deg, #f5f7fa 0%, #e4e8eb 100%);
          backdrop-filter: blur(10px);
          color: #2c3e50;
        }

        .welcome-title {
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
          font-size: 3rem;
          font-weight: 700;
          background: linear-gradient(to right, #3498db, #00aced);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          margin-bottom: 25px;
        }
        
        .tab-item p {
          margin-bottom: 15px;
          font-size: 1.8rem;
          transition: all 0.3s ease;
        }
        
        #vulnerability-tab p:hover {
          transform: translateX(8px);
          color: #3498db;; 
        }
        
        #connectivity-tab p:hover {
          transform: translateX(8px);
          color: #3498db;;
        }
        
        #risk-tab p:hover {
          transform: translateX(8px);
          color: #3498db;;
        }
        
        #predictors-tab p:hover {
          transform: translateX(8px);
          color: #3498db;;
        }
        
        .welcome-footer {
          margin-top: 30px;
          font-style: italic;
          color: #7f8c8d;
          border-top: 1px dashed #bdc3c7;
          padding-top: 20px;
          font-size: 1.5rem;
        }
        
        .external-link-button {
          color: white;
          border: none;
          padding: 8px 15px;
          border-radius: 5px;
          cursor: pointer;
          text-decoration: none !important;
          font-size: 1.0rem;
          transition: background-color 0.3s ease;
        }
        
      "))
    ),
    
    tabItems(
      tabItem(tabName = "home",
              div(class = "welcome-container",
                  div(class = "welcome-box",
                      div(class = "welcome-title", "Welcome!"),
                      div(class = "welcome-text",
                          p("This dashboard helps visualize and analyze MRT station vulnerability and connectivity in Singapore"),
                          p("Explore the following sections:"),
                          div(class = "tab-item", id = "vulnerability-tab",
                            p(strong("Vulnerability Map:"), "View stations based on risk of breakdowns")
                          ),
                          div(class = "tab-item", id = "connectivity-tab",
                            p(strong("Connectivity Map:"), "Explore how well-connected each station is")
                          ),
                          div(class = "tab-item", id = "risk-tab",
                            p(strong("Risk Analysis:"), "Get a quick overview of key stations needing attention")
                          ),
                          div(class = "tab-item", id = "predictors-tab",
                              p(strong("Important Predictors:"), "Find out more about key variables")
                          ),
                      div(class = "welcome-footer",
                          "Built for insight-driven and resilient transport planning",
                          tags$div(class = "external-link-button",
                                   tags$a(href = "https://github.com/gotoshleep/DSE3101-choochootrain-.git",
                                          target = "_blank",
                                          class = "btn btn-primary",
                                          "Go to our Github Website"))
                      )
                      
                )
            )
      )),

    
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
                    title = "Top 5 Vulnerable Stations",
                    DTOutput("top_vulnerable_table1"))
              )
      ),
      
      tabItem(tabName = "map2",
              fluidRow(
                
                box(width = 8, leafletOutput("connectivity_map",
                                             height = "600px")),
                
                box(width = 4, 
                    title = "Top # stations(max 20):", background = "light-blue"
                )
              )
      ),
      
      tabItem(tabName = "analysis",
              fluidRow(
                box(width = 6,
                    title = "Top 5 Most Vulnerable Stations",
                    DTOutput("top_vulnerable_table")),
                box(width = 6,
                    title = "Top 5 Least Connected Stations",
                    DTOutput("low_connectivity_table"))
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
latlng_data <- read_xlsx("MRT_DATA.xlsx")
vul_data<-v_df %>% arrange(station, day_type,is_peak) %>% ## ensure that it is ordered by station_code, then weekday then peak status 
  mutate(status = paste0(day_type,is_peak)) %>%
  select(-day_type, -is_peak, -avg_score, -line_code, -X) %>%
  pivot_wider(names_from = status, values_from = vul_category) %>%
  mutate(information = paste(station_code," ", station, 
                             "<br>Vulnerability quantiles:<br>", 
                             "Weekday Offpeak: ", WEEKDAY0, 
                             "<br>Weekday Peak: ", WEEKDAY1, 
                             "<br>Weekend/Holiday Offpeak: ", `WEEKENDS/HOLIDAY0`
  )) %>% #create data for the pop up
  mutate(station_w_code = paste(station_code," ", station))%>%
  left_join(y = latlng_data, by = "station_code") #add lat long data to vulnerability data

v_df <- left_join(x=v_df, y = latlng_data, by = "station_code") #add lat long data to vulnerability data
input <- data.frame(day_of_week = c("WEEKDAY"), peak_bool = c(1))

###initialise the top 5 most vulnerable stations. 
top_vul <- v_df %>% arrange(desc(avg_score)) %>% filter(day_type == input$day_of_week & is_peak == input$peak_bool) %>% head(5)
##############################################################################################################################

####################################Connectivity map####################################
c_df <- read.csv("connectivity_score.csv")
connect_data<-c_df %>%
  select(station_code, stations, Score) %>%
  group_by(stations) %>% 
  summarize(
    Score = mean(Score),
    across(station_code, ~paste(., collapse = "/")) 
  ) %>%
  ungroup() %>%
  mutate(Score = as.numeric(Score)*5) %>%
  mutate(station_w_code = paste(station_code, stations)) %>%
  mutate(information = paste(station_w_code,
                             "<br>Connectivity Score:<br>", Score
  )) %>% #create data for the pop up
  inner_join(y = latlng_data, by = "stations") #add lat long data to vulnerability data


c_df <- left_join(x=c_df, y = latlng_data, by = "station_code") #add lat long data to vulnerability data
input <- data.frame(day_of_week = c("WEEKDAY"), peak_bool = c(1))

bot5_connect <- c_df %>% arrange(Score) %>% head(5)
##############################################################################################################################

least_connected <- c_df %>%
  arrange(Score) %>%
  head(5) %>%
  mutate(Score= round(Score,2))

most_vulnerable <- v_df %>%
  na.omit()%>% 
  group_by(station, station_code) %>% 
  summarize(mean_score = mean(avg_score)) %>%
  arrange(desc(mean_score))%>%
  head(5) %>%
  mutate(mean_score = round(mean_score,2))

server <- function(input, output, session) {
  
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
                       label = ~station_w_code, 
                       popup = ~information,
                       fillOpacity = 1,
                       fillColor = ~ifelse(station %in% top_vul$station , "red","white"),
                       opacity = 1, 
                       color = "black", 
                       radius = ~ifelse(station %in% top_vul$station , 6,4), 
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
                       label = ~station_w_code, 
                       popup = ~information,
                       fillOpacity = 1,
                       fillColor = ~ifelse(stations %in% bot5_connect$stations.x , "red","white"),
                       opacity = 1, 
                       color = "black", 
                       radius = ~ifelse(stations %in% bot5_connect$stations.x, 6,4), 
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
    
    v_df %>%
      filter(day_type == input$day_of_week, 
             is_peak == input$peak_bool) %>%
      arrange(desc(avg_score)) %>%
      head(input$top_number) %>%
      select(station_code, station, avg_score) %>%
      mutate(avg_score = round(avg_score, 2)) %>%
      rename("Station Code" = station_code,
             "Station" = station,
             "Vulnerability Score" = avg_score)
  }, ignoreNULL = FALSE)  # Set to FALSE to run on app initialization
  
  output$top_vulnerable_table1 <- renderDT({
    vulnerable_reactive_table() %>%
      datatable(
        options = list(
          dom = 't', 
          pageLength = 5,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE
      )
  })
  ################################################################################
  ################################################################################
  
  ################################################################################
  ## Table(top5 stations) for TAB3
  ################################################################################
  #Top 5 Least Connected Stations
  output$low_connectivity_table <- renderDT({
    data = least_connected %>%
      select(station_code, stations, Score) %>%
      rename("Station" = stations,
             "Station_Code" = station_code)
    datatable(data,
              options = list(dom = 't', pageLength = 5))
  })
  
  #Top 5 most Vulnerable stations overall
  output$top_vulnerable_table <- renderDT({
    data = most_vulnerable %>%
      select(station_code, station, mean_score) %>%
      rename("Station" = station,
             "Station_Code" = station_code)
    datatable(data,
              options = list(dom = 't', pageLength = 5))
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
        label = ~station_w_code,
        popup = ~information,
        fillOpacity = 1,
        fillColor = ~ifelse(station %in% top_vul$station, "red", "white"),
        color = "black",
        radius = ~ifelse(station %in% top_vul$station, 6, 4),
        weight = 1.5
      )
  })
  
  output$data_table <- renderTable({
    data()  # Display current data
  })
}



shinyApp(ui = ui, server = server)

