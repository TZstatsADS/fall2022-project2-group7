library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shiny)
library(shinydashboard)


vehicle_collisions_df = read.csv("VehicleCollisions.csv") %>% drop_na()
colnames(vehicle_collisions_df) <- gsub("\\.","_",colnames(vehicle_collisions_df))



vehicle_collisions_df$CRASH_DATE <- as.Date(vehicle_collisions_df$CRASH_DATE, format = "%m/%d/%Y")
vehicle_collisions_df$CRASH_TIME <- as.POSIXct(vehicle_collisions_df$CRASH_TIME, format = "%H:%M")

vehicle_collisions_df <- vehicle_collisions_df %>% mutate(Year = as.numeric(format(CRASH_DATE,'%Y')), Month = as.numeric(format(CRASH_DATE,'%m')), Hour = as.numeric(format(as.POSIXct(CRASH_TIME), format = "%H"))) %>% filter(Year>2016)


# Define UI ----

body <- dashboardBody(
  
  tabItems(
    # ------------------ Home ----------------------------------------------------------------
    
    tabItem(tabName = "Home", fluidPage(
      fluidRow(box(width = 20, title = "Introduction", status = "primary",
                   solidHeader = TRUE, h3("Vehicle Accidents"),
                   h4("By Chaitanya Shah, Weilin Zhou, John Podias"),
                   h5("Drawing data from multiple sources, this application provides insight into the economic impact of coronavirus 2019 (COVID-19) on New York’s city economy. The results shed light on both the financial fragility of many businesses, and the significant impact COVID-19 had on these businesses in the weeks after the COVID-19–related disruptions began."),
                   h5("The application will mainly track down the change in the number of businesses being closed or newly opened across Covid timeline. We divided the businesses into 4 types:", strong("Retail, Service, Food and Beverage, Entertainment"))))

    )), # end of home 
    # ------------------ Boroughs-----------------------------------
    tabItem(tabName = "Boroughs",
            h2("How Many Accidents occured In Each Neighborhood?", align = 'center'),
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
            h2("Different types of accidents occured", align = 'center'),
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
            h2("Different Times when accidents occured", align = 'center'),
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
    )
  )
)


ui <- dashboardPage(
  title="Vehicle Accidents",
  skin = "black", 
  dashboardHeader(title=span("Vehicle Accidents",style="font-size: 20px"),disable = FALSE),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home"),
      menuItem("Boroughs", tabName = "Boroughs"),
      menuItem("Type of Accidents", tabName = "Type"),
      menuItem("Time of Accidents", tabName = "Time")
  )),
  body 
)


server <- function(input, output,session) {
  
  session$allowReconnect("force")
  
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
    
  }, height = 750, width = 900)
  
  ###############################################################  
  
  output$Plot2 <- renderPlot({ 
    
    if(input$borough2 == "All"){
      
      vehicle_analysis <- vehicle_collisions_df %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>50) %>% filter(VEHICLE_TYPE_CODE_1 != "")
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
      
   }
    else if(input$borough2 == "Bronx"){
      
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BRONX")
      
      vehicle_analysis <- district_analysis_borugh %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>50) %>% filter(VEHICLE_TYPE_CODE_1 != "") 
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
  }
    else if(input$borough2== "Manhattan"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "MANHATTAN")
      
      vehicle_analysis <- district_analysis_borugh %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>50) %>% filter(VEHICLE_TYPE_CODE_1 != "")
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
      
  }
    else if(input$borough2 == "Brooklyn"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BROOKLYN")
      
      vehicle_analysis <- district_analysis_borugh %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>50) %>% filter(VEHICLE_TYPE_CODE_1 != "")
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
      
  }
    else if(input$borough2 == "Queens"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "QUEENS")
      
      vehicle_analysis <- district_analysis_borugh %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>50) %>% filter(VEHICLE_TYPE_CODE_1 != "")
      
      ggplot(vehicle_analysis,aes(x=factor(Year),count,fill=VEHICLE_TYPE_CODE_1)) + geom_histogram(stat = "identity") + labs(y="Number of different vehicle collisions happened",x="Year") + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) 
        }
    else if(input$borough2 == "Staten Island"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "STATEN ISLAND")
      
      vehicle_analysis <- district_analysis_borugh %>% group_by(VEHICLE_TYPE_CODE_1,Year) %>% summarise(count=n()) %>% filter(count>50) %>% filter(VEHICLE_TYPE_CODE_1 != "")
      
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
    
  }, height = 750, width = 900)
  
}

shinyApp(ui, server)