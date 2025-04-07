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
  dashboardHeader(title = span(tagList(icon("train"), "MRT Station Risk"))),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Vulnerability Map", tabName = "map", icon = icon("exclamation-triangle")),
      menuItem("Connectivity Map", tabName = "map2", icon = icon("project-diagram")),
      menuItem("Risk Analysis", tabName = "analysis", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Custom modal styling */
        .modal-content {
          border-radius: 10px;
        }
        .modal-header {
          background-color: #3c8dbc;
          color: white;
          border-top-left-radius: 10px;
          border-top-right-radius: 10px;
        }
        /* Plot styling */
        #stationScorePlot {
         margin-top: 15px;
       }
      "))
    ),
    
    tabItems(
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
    
    # color_palette <- colorNumeric(
    #   palette = c("red", "yellow", "green"),
    #   domain = connectivity_data_map$Score
    # )
    
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
  
  # vulnerability_data <- read.csv("vul_scores_full.csv")
  # 
  # output$top_vulnerable_table <- renderDT({
  #   most_vulnerable <- vulnerability_data %>%
  #     arrange(desc(vul_scores)) %>%
  #     select(station, vul_scores) %>%
  #     head(5)
  #   datatable(most_vulnerable,
  #             options = list(dom = 't', pageLength = 5))
  # })
  
  
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

