
#library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shinydashboard)
library(fresh) 
library(devtools)
install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethr)
library(choroplethrZip)
#library(rsconnect)


vehicle_collisions_df = read.csv("VehicleCollisions.csv") %>% drop_na()
colnames(vehicle_collisions_df) <- gsub("\\.","_",colnames(vehicle_collisions_df))



vehicle_collisions_df$CRASH_DATE <- as.Date(vehicle_collisions_df$CRASH_DATE, format = "%m/%d/%Y")
vehicle_collisions_df$CRASH_TIME <- as.POSIXct(vehicle_collisions_df$CRASH_TIME, format = "%H:%M")

vehicle_collisions_df <- vehicle_collisions_df %>% mutate(Year = as.numeric(format(CRASH_DATE,'%Y')), Month = as.numeric(format(CRASH_DATE,'%m')), Hour = as.numeric(format(as.POSIXct(CRASH_TIME), format = "%H"))) %>% filter(Year>2016)


# Define UI ----

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "180px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

body <- dashboardBody(
  use_theme(mytheme),
  tabItems(
    # ------------------ Introduction ----------------------------------------------------------------
    
    tabItem(tabName = "Introduction", fluidPage(
      fluidRow(box(width = 20, title = "Introduction", status = "primary",
                   solidHeader = TRUE, h3("NYC Motor Vehicle Accidents"),
                   h4("By Chaitanya Shah, Weilin Zhou, John Podias"),
                   h5(strong("Dataset:"), "The dataset is taken from NYC Open Data and sourced from the NYPD. It is a collection of NYC motor-vehicle collisions where each row is a collision. It contains multiple data fields that describe the collisions such as vehicle type, date of accidents, zip code, borough, reason for accident, number of deaths, etc."),
                   h5(strong("Collection of Data:"), "The citywide traffic safety initiative, Vision Zero, started in the year 2014 and has pushed for further data collection. NYPD officers now document the information to be stored in a database using the The Finest Online Records Management System (FORMS). It is updated daily."),
                   h5(strong("Purpose of Tool:"), "This tool provides the ability to explore traffic patterns through the years and see how COVID has affected it. It can be used by a number of different audiences. Primarily, it can used by the NYPD & Vision Zero (the citywide safety traffic initiative) for the purposes of improving safety. It can even be used by NYC commuters to plan optimal routes, or  prospective home buyers to explore safer neighborhoods. Users can analyze the initial extreme COVID effects in 2020, but also see what persists today since the data is updated daily. We know COVID has changed travel due to things like remote work and limited flights, but there are different ways to explore it")
      )))
    ), # end of home 
    # ------------------ Map-----------------------------------
    tabItem(tabName = "Map",
            h2("Accidents by Zip Code", align = 'center'),
            sidebarLayout(position = "left", 
                          sidebarPanel(
                            h4("Filter"),
                            
                            # widget for crime type
                            selectInput(inputId = "year",
                                        label = "Choose a year:",
                                        choices = c("All","2017", "2018", "2019", "2020", "2021","2022"))
                          ),
                          mainPanel(
                            plotOutput(outputId = "Plot7")
                          )
            )
    ),
    # ------------------ Boroughs-----------------------------------
    tabItem(tabName = "Boroughs",
            h2("Accidents by Borough", align = 'center'),
            sidebarLayout(position = "left", 
                          sidebarPanel(
                            h4("Filter"),
                            
                            # widget for crime type
                            selectInput(inputId = "borough",
                                        label = "Choose a borough:",
                                        choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island","All"))
                          ),
                          mainPanel(
                            plotOutput(outputId = "Plot1")
                          )
            )
    ),
    # ------------------ Type of Accidents-----------------------------------
    tabItem(tabName = "Type",
            h2("Types of Accidents", align = 'center'),
            sidebarLayout(position = "left", 
                          sidebarPanel(
                            h4("Filter"),
                            
                            # widget for crime type
                            selectInput(inputId = "borough2",
                                        label = "Choose a borough:",
                                        choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island","All"))
                          ),
                          mainPanel(
                            plotOutput(outputId = "Plot2")
                          )
            )
    ),
    # ------------------ Time of Accidents-----------------------------------
    tabItem(tabName = "Time",
            h2("Time of Accidents", align = 'center'),
            sidebarLayout(position = "left", 
                          sidebarPanel(
                            h4("Filter"),
                            
                            # widget for crime type
                            selectInput(inputId = "borough3",
                                        label = "Choose a borough:",
                                        choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island","All"))
                          ),
                          mainPanel(
                            plotOutput(outputId = "Plot3")
                          )
            )
    ),
    tabItem(tabName = "Casualities",
            h2("Victims of Accidents", align = 'center'),
            sidebarLayout(position = "left", 
                          sidebarPanel(
                            h4("Filter"),
                            
                            # widget for crime type
                            selectInput(inputId = "borough4",
                                        label = "Choose a borough:",
                                        choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island","All")),
                            selectInput(inputId = "harm",
                                        label = "Choose a type of harm:",
                                        choices = c("Injured", "Death", "Both")),
                            selectInput(inputId = "victim",
                                        label = "Choose a type of victim:",
                                        choices = c("Pedestrians", "Cyclist", "Motorist","All"))
                          ),
                          mainPanel(
                            plotOutput(outputId = "Plot4")
                          )
            )
    ),
    
    
    # ------------------ References ----------------------------------------------------------------
    
    tabItem(tabName = "References", fluidPage(
      fluidRow(box(width = 20, title = "References", status = "primary",
                   solidHeader = TRUE, h5(strong("Dataset:"), "https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95"),
                   h5(strong("Contributors:"), "Chaitanya Shah (css2211@columbia.edu), Weilin Zhou (wz2563@columbia.edu), John Podias (jep2215@columbia.edu)")
      )))
    ) # end of reference
    
  )
)


ui <- dashboardPage(
  title="NYC Vehicle Accidents",
  skin = "black", 
  dashboardHeader(title="NYC Motor Vehicle Accidents"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction"),
      menuItem("Map", tabName = "Map"),
      menuItem("Boroughs", tabName = "Boroughs"),
      menuItem("Type of Accidents", tabName = "Type"),
      menuItem("Time of Accidents", tabName = "Time"),
      menuItem("Casualities of Accidents", tabName = "Casualities"),
      menuItem("References", tabName = "References")
    )),
  body
)


server <- function(input, output) {
  
  #session$allowReconnect("force")
  
  output$Plot1 <- renderPlot({ 
    
    if(input$borough == "All"){
      district_analysis2 <- vehicle_collisions_df %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20), axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) 
    }
    else if(input$borough == "Bronx"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BRONX")
      district_analysis2 <- district_analysis_borugh %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20))   
    }
    else if(input$borough == "Manhattan"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "MANHATTAN")
      district_analysis2 <- district_analysis_borugh %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20))   
    }
    else if(input$borough == "Brooklyn"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BROOKLYN")
      district_analysis2 <- district_analysis_borugh %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white")  + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20))   
    }
    else if(input$borough == "Queens"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "QUEENS")
      district_analysis2 <- district_analysis_borugh %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) 
    }
    else if(input$borough == "Staten Island"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "STATEN ISLAND")
      district_analysis2 <- district_analysis_borugh %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) 
    }
    
  }, height = 750, width = 800)
  
  ###############################################################  
  
  output$Plot7 <- renderPlot({ 
    
    if(input$year == "All"){
      vehicle_analysis_for_map_collisions <- vehicle_collisions_df%>% rename(region = ZIP_CODE)
      vehicle_analysis_for_map_collisions<-vehicle_analysis_for_map_collisions %>%  group_by(region) %>% summarise(value=n())
      vehicle_analysis_for_map_collisions$region<-as.character(vehicle_analysis_for_map_collisions$region)
      zip_choropleth(vehicle_analysis_for_map_collisions, legend = "Number of Collisions", county_zoom =  c(36005, 36047, 36061, 36081, 36085)) + theme( legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
      
    }
    else if(input$year == "2017"){
      vehicle_collisions_df_year <- filter(vehicle_collisions_df,Year == 2017)
      vehicle_analysis_for_map_collisions <- vehicle_collisions_df_year%>% rename(region = ZIP_CODE)
      vehicle_analysis_for_map_collisions<-vehicle_analysis_for_map_collisions %>%  group_by(region) %>% summarise(value=n())
      vehicle_analysis_for_map_collisions$region<-as.character(vehicle_analysis_for_map_collisions$region)
      zip_choropleth(vehicle_analysis_for_map_collisions, legend = "Number of Collisions", county_zoom =  c(36005, 36047, 36061, 36081, 36085)) + theme( legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$year == "2018"){
      vehicle_collisions_df_year <- filter(vehicle_collisions_df,Year == 2017)
      vehicle_analysis_for_map_collisions <- vehicle_collisions_df_year%>% rename(region = ZIP_CODE)
      vehicle_analysis_for_map_collisions<-vehicle_analysis_for_map_collisions %>%  group_by(region) %>% summarise(value=n())
      vehicle_analysis_for_map_collisions$region<-as.character(vehicle_analysis_for_map_collisions$region)
      zip_choropleth(vehicle_analysis_for_map_collisions, legend = "Number of Collisions", county_zoom =  c(36005, 36047, 36061, 36081, 36085)) + theme( legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$year == "2019"){
      vehicle_collisions_df_year <- filter(vehicle_collisions_df,Year == 2017)
      vehicle_analysis_for_map_collisions <- vehicle_collisions_df_year%>% rename(region = ZIP_CODE)
      vehicle_analysis_for_map_collisions<-vehicle_analysis_for_map_collisions %>%  group_by(region) %>% summarise(value=n())
      vehicle_analysis_for_map_collisions$region<-as.character(vehicle_analysis_for_map_collisions$region)
      zip_choropleth(vehicle_analysis_for_map_collisions, legend = "Number of Collisions", county_zoom =  c(36005, 36047, 36061, 36081, 36085)) + theme( legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$year == "2020"){
      vehicle_collisions_df_year <- filter(vehicle_collisions_df,Year == 2017)
      vehicle_analysis_for_map_collisions <- vehicle_collisions_df_year%>% rename(region = ZIP_CODE)
      vehicle_analysis_for_map_collisions<-vehicle_analysis_for_map_collisions %>%  group_by(region) %>% summarise(value=n())
      vehicle_analysis_for_map_collisions$region<-as.character(vehicle_analysis_for_map_collisions$region)
      zip_choropleth(vehicle_analysis_for_map_collisions, legend = "Number of Collisions", county_zoom =  c(36005, 36047, 36061, 36081, 36085)) + theme( legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$year == "2021"){
      vehicle_collisions_df_year <- filter(vehicle_collisions_df,Year == 2017)
      vehicle_analysis_for_map_collisions <- vehicle_collisions_df_year%>% rename(region = ZIP_CODE)
      vehicle_analysis_for_map_collisions<-vehicle_analysis_for_map_collisions %>%  group_by(region) %>% summarise(value=n())
      vehicle_analysis_for_map_collisions$region<-as.character(vehicle_analysis_for_map_collisions$region)
      zip_choropleth(vehicle_analysis_for_map_collisions, legend = "Number of Collisions", county_zoom =  c(36005, 36047, 36061, 36081, 36085)) + theme( legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$year == "2022"){
      vehicle_collisions_df_year <- filter(vehicle_collisions_df,Year == 2017)
      vehicle_analysis_for_map_collisions <- vehicle_collisions_df_year%>% rename(region = ZIP_CODE)
      vehicle_analysis_for_map_collisions<-vehicle_analysis_for_map_collisions %>%  group_by(region) %>% summarise(value=n())
      vehicle_analysis_for_map_collisions$region<-as.character(vehicle_analysis_for_map_collisions$region)
      zip_choropleth(vehicle_analysis_for_map_collisions, legend = "Number of Collisions", county_zoom =  c(36005, 36047, 36061, 36081, 36085)) + theme( legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    
    
    
  }, height = 750, width = 750)
  
  ###############################################################  
  
  output$Plot2 <- renderPlot({ 
    
    if(input$borough2 == "All"){
      
      vehicle_analysis <- vehicle_collisions_df %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>500) %>% filter(VEHICLE_TYPE_CODE_1 != "")
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
      
    }
    else if(input$borough2 == "Bronx"){
      
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BRONX")
      
      vehicle_analysis <- district_analysis_borugh %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>150) %>% filter(VEHICLE_TYPE_CODE_1 != "") 
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$borough2== "Manhattan"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "MANHATTAN")
      
      vehicle_analysis <- district_analysis_borugh %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>150) %>% filter(VEHICLE_TYPE_CODE_1 != "")
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
      
    }
    else if(input$borough2 == "Brooklyn"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BROOKLYN")
      
      vehicle_analysis <- district_analysis_borugh %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>150) %>% filter(VEHICLE_TYPE_CODE_1 != "")
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
      
    }
    else if(input$borough2 == "Queens"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "QUEENS")
      
      vehicle_analysis <- district_analysis_borugh %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>150) %>% filter(VEHICLE_TYPE_CODE_1 != "")
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$borough2 == "Staten Island"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "STATEN ISLAND")
      
      vehicle_analysis <- district_analysis_borugh %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>150) %>% filter(VEHICLE_TYPE_CODE_1 != "")
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    
  }, height = 750, width = 900)
  
  
  ###############################################################  
  
  output$Plot3 <- renderPlot({ 
    
    if(input$borough3 == "All"){
      
      daytime_analysis <- vehicle_collisions_df%>%
        mutate(Daytime = ifelse(Hour %in% 6:12, "Morning",
                                ifelse(Hour %in% 13:18, "Afternoon",
                                       ifelse(Hour %in% 19:23, "Evening","Midnight"
                                       ))))%>%
        group_by(Daytime,Year,Month)%>%
        summarise(count=n())
      
      
      ggplot(daytime_analysis,aes(x=factor(Year),count,fill=Daytime))+
        geom_histogram(stat = "identity",position="dodge")+
        labs(y="Number of different time when collisions happened",
             x="Month") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$borough3 == "Bronx"){
      
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BRONX")
      
      daytime_analysis <- district_analysis_borugh%>%
        mutate(Daytime = ifelse(Hour %in% 6:12, "Morning",
                                ifelse(Hour %in% 13:18, "Afternoon",
                                       ifelse(Hour %in% 19:23, "Evening","Midnight"
                                       ))))%>%
        group_by(Daytime,Year,Month)%>%
        summarise(count=n())
      
      
      ggplot(daytime_analysis,aes(x=factor(Year),count,fill=Daytime))+
        geom_histogram(stat = "identity",position="dodge")+
        labs(y="Number of different time when collisions happened",
             x="Month") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$borough3== "Manhattan"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "MANHATTAN")
      
      
      daytime_analysis <- district_analysis_borugh%>%
        mutate(Daytime = ifelse(Hour %in% 6:12, "Morning",
                                ifelse(Hour %in% 13:18, "Afternoon",
                                       ifelse(Hour %in% 19:23, "Evening","Midnight"
                                       ))))%>%
        group_by(Daytime,Year,Month)%>%
        summarise(count=n())
      
      
      ggplot(daytime_analysis,aes(x=factor(Year),count,fill=Daytime))+
        geom_histogram(stat = "identity",position="dodge")+
        labs(y="Number of different time when collisions happened",
             x="Month") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$borough3 == "Brooklyn"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BROOKLYN")
      
      
      daytime_analysis <- district_analysis_borugh%>%
        mutate(Daytime = ifelse(Hour %in% 6:12, "Morning",
                                ifelse(Hour %in% 13:18, "Afternoon",
                                       ifelse(Hour %in% 19:23, "Evening","Midnight"
                                       ))))%>%
        group_by(Daytime,Year,Month)%>%
        summarise(count=n())
      
      
      ggplot(daytime_analysis,aes(x=factor(Year),count,fill=Daytime))+
        geom_histogram(stat = "identity",position="dodge")+
        labs(y="Number of different time when collisions happened",
             x="Month") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$borough3 == "Queens"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "QUEENS")
      
      daytime_analysis <- district_analysis_borugh%>%
        mutate(Daytime = ifelse(Hour %in% 6:12, "Morning",
                                ifelse(Hour %in% 13:18, "Afternoon",
                                       ifelse(Hour %in% 19:23, "Evening","Midnight"
                                       ))))%>%
        group_by(Daytime,Year,Month)%>%
        summarise(count=n())
      
      
      ggplot(daytime_analysis,aes(x=factor(Year),count,fill=Daytime))+
        geom_histogram(stat = "identity",position="dodge")+
        labs(y="Number of different time when collisions happened",
             x="Month") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    else if(input$borough3 == "Staten Island"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "STATEN ISLAND")
      
      
      daytime_analysis <- district_analysis_borugh%>%
        mutate(Daytime = ifelse(Hour %in% 6:12, "Morning",
                                ifelse(Hour %in% 13:18, "Afternoon",
                                       ifelse(Hour %in% 19:23, "Evening","Midnight"
                                       ))))%>%
        group_by(Daytime,Year,Month)%>%
        summarise(count=n())
      
      
      ggplot(daytime_analysis,aes(x=factor(Year),count,fill=Daytime))+
        geom_histogram(stat = "identity",position="dodge")+
        labs(y="Number of different time when collisions happened",
             x="Month") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
    }
    
  }, height = 750, width = 800)
  
  
  
  output$Plot4 <- renderPlot({ 
    
    if(input$borough4 == "All"){
      if (input$harm == "Death"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_KILLED))%>%
            filter(count>1)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_KILLED))%>%
            filter(count>1)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_KILLED))%>%
            filter(count>1)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_KILLED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Injured"){
        
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>100)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>150)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Both"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED)+sum(NUMBER_OF_PEDESTRIANS_KILLED ) )%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- vehicle_collisions_df%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>100)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
        
      }
      
    }
    else if(input$borough4 == "Bronx"){
      
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BRONX")
      
      if (input$harm == "Death"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_KILLED))%>%
            filter(count>1)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Injured"){
        
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>20)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Both"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED)+sum(NUMBER_OF_PEDESTRIANS_KILLED ) )%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>20)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
        
      }
      
    }
    else if(input$borough4 == "Brooklyn"){
      
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BROOKLYN")
      
      if (input$harm == "Death"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_KILLED))%>%
            filter(count>1)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Injured"){
        
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>20)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Both"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED)+sum(NUMBER_OF_PEDESTRIANS_KILLED ) )%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>20)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
        
      }
      
    }
    
    else if(input$borough4== "Manhattan"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "MANHATTAN")
      
      
      if (input$harm == "Death"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_KILLED))%>%
            filter(count>1)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Injured"){
        
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>20)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Both"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED)+sum(NUMBER_OF_PEDESTRIANS_KILLED ) )%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>20)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
        
      }
    }
    else if(input$borough4 == "Queens"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "QUEENS")
      
      if (input$harm == "Death"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_KILLED))%>%
            filter(count>1)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Injured"){
        
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>20)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Both"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED)+sum(NUMBER_OF_PEDESTRIANS_KILLED ) )%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>20)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
        
      }
    }
    else if(input$borough4 == "Staten Island"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "STATEN ISLAND")
      
      
      if (input$harm == "Death"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_KILLED))%>%
            filter(count>0)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_KILLED))%>%
            filter(count>1)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Injured"){
        
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>20)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
      }
      else if(input$harm == "Both"){
        if (input$victim == "Pedestrians"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PEDESTRIANS_INJURED)+sum(NUMBER_OF_PEDESTRIANS_KILLED ) )%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Cyclist"){
          
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_CYCLIST_INJURED))%>%
            filter(count>5)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "Motorist"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_MOTORIST_INJURED))%>%
            filter(count>20)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        else if (input$victim == "All"){
          vehicle_analysis <- district_analysis_borugh%>%
            group_by(VEHICLE_TYPE_CODE_1,Year)%>%
            summarise(count=sum(NUMBER_OF_PERSONS_INJURED))%>%
            filter(count>50)%>%
            filter(VEHICLE_TYPE_CODE_1 != "")
          
          ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1))+
            geom_histogram(stat = "identity") +
            labs(y="Number of different vehicle collisions happened", x="Year")  + 
            theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
          
          
        }
        
        
      }
    }
    
  }, height = 750, width = 900)
  
}

shinyApp(ui, server)
