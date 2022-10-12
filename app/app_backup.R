library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shiny)


vehicle_collisions_df = read.csv("VehicleCollisions.csv")%>%
  drop_na()
colnames(vehicle_collisions_df) <- gsub("\\.","_",colnames(vehicle_collisions_df))



vehicle_collisions_df$CRASH_DATE <- as.Date(vehicle_collisions_df$CRASH_DATE, format = "%m/%d/%Y")
vehicle_collisions_df$CRASH_TIME <- as.POSIXct(vehicle_collisions_df$CRASH_TIME, format = "%H:%M")

vehicle_collisions_df <- vehicle_collisions_df%>%
  mutate(Year = as.numeric(format(CRASH_DATE,'%Y')),
         Month = as.numeric(format(CRASH_DATE,'%m')),
         Hour = as.numeric(format(as.POSIXct(CRASH_TIME), format = "%H")))%>%
  filter(Year>2016)


# Define UI ----



ui <- fluidPage(
  # App title ----
  titlePanel("Accidents Borough Wise"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select for the borough ----
      selectInput(inputId = "borough",
                  label = "Choose a borough:",
                  choices = c("Overall","Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island",""))
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: tsPlot on borough ----
      plotOutput(outputId = "tsPlot1")
    )
  )
)

server <- function(input, output) {
  
  output$tsPlot1 <- renderPlot({ 
    
    if(input$borough == "Overall"){
      district_analysis2 <- vehicle_collisions_df %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20))   
    }
    else if(input$borough == "Bronx"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BRONX")
      district_analysis2 <- district_analysis_borugh %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20))   
    }
    else if(input$borough == "Manhattan"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "MANHATTAN")
      district_analysis2 <- district_analysis_borugh %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20))   
    }
    else if(input$borough == "Brooklyn"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "BROOKLYN")
      district_analysis2 <- district_analysis_borugh %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white")  + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20))   
    }
    else if(input$borough == "Queens"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "QUEENS")
      district_analysis2 <- district_analysis_borugh %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20))   
    }
    else if(input$borough == "Staten Island"){
      district_analysis_borugh <- filter(vehicle_collisions_df,BOROUGH == "STATEN ISLAND")
      district_analysis2 <- district_analysis_borugh %>% group_by(Year)%>% summarise(count = n())
      ggplot(district_analysis2, aes(x="", y=count, fill=factor(Year))) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + theme(axis.title.x = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_text(size = 20))   
    }
    
  }, height = 800, width = 1000)
}

shinyApp(ui, server)