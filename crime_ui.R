#install.packages("dygraphs")
#install.packages("xts")
#install.packages("plotly")
#install.packages("leaflet.extras")
library(shiny)
library(shinydashboard)
library(leaflet)
library(dygraphs)
library(dplyr)
library(xts)
library(ggplot2)
library(leaflet.extras)
library(lubridate)
library(plotly)
library(stringr)

#read in data and clean
Boston_crime <- read.csv("Boston_crime.csv", stringsAsFactors=FALSE)
count <- read.csv("crime_count.csv", stringsAsFactors=FALSE)
count_crime <- read.csv("count_crime.csv", stringsAsFactors=FALSE)
count$Crime_Type <- as.character(count$Crime_Type)
count$Date <- as.Date(count$Date, "%Y-%m-%d")
All_totals <- read.csv("All_totals.csv")
monthly_2018 <- read.csv("2018_monthtot.csv", stringsAsFactors=FALSE)
monthly_2018$Date <- as.Date(monthly_2018$Date, "%Y-%m-%d")

#2015 Data
crime_2015 <- read.csv("july_2015.csv", stringsAsFactors=FALSE)
crime_2015[crime_2015==1.0] <- NA
crime_2015_clean <- na.omit(crime_2015)
BostonDate = str_split_fixed(crime_2015_clean$OCCURRED_ON_DATE, " ", 2)
crime_2015_clean <- cbind(crime_2015_clean, BostonDate)
colnames(crime_2015_clean)[colnames(crime_2015_clean) == '1'] <- 'Date'
colnames(crime_2015_clean)[colnames(crime_2015_clean) == '2'] <- 'Time'
crime_2015_clean$Date <- as.Date(crime_2015_clean$Date, "%m/%d/%Y")

monthly_2015 <- read.csv("month_2015.csv")


#clean data 2018
Boston_crime[Boston_crime==-1.0] <- NA
Boston_crime_clean <- na.omit(Boston_crime)
Boston_date = str_split_fixed(Boston_crime_clean$OCCURRED_ON_DATE, " ", 2)
Boston_crime_clean <- cbind(Boston_crime_clean, Boston_date)
colnames(Boston_crime_clean)[colnames(Boston_crime_clean) == '1'] <- 'Date'
colnames(Boston_crime_clean)[colnames(Boston_crime_clean) == '2'] <- 'Time'
Boston_crime_clean$Date <- as.Date(Boston_crime_clean$Date, "%m/%d/%Y")
weekly_crime <- read.csv("weekly_crime.csv")


choices2 <- as.character(unique(Boston_crime_clean$OFFENSE_CODE_GROUP))
choices2 <- c('All', choices2)

choices3 <- as.character(unique(Boston_crime_clean$OFFENSE_CODE_GROUP))

choices_2015 <- as.character(unique(crime_2015_clean$OFFENSE_CODE_GROUP))
choices_2015 <- c('All', choices_2015)

ui <- dashboardPage(
  dashboardHeader(title = "Boston Crime Data"),
  dashboardSidebar(sidebarMenu(
    menuItem("April 2018 Crime", tabName = "Crime_2018", icon = icon("signal"),
      menuSubItem("2018 Maps", tabName = "Maps_2018"),
      menuSubItem("2018 Graphs", tabName = "Graphs_2018")),
    menuItem("July 2015 Data", tabName = "Crime_2015", icon = icon("th"))
  )),
dashboardBody(tabItems(
  tabItem(tabName = "Maps_2018",
          fluidRow(
            box(
            width = 12,
            title = "Boston Crime Data",
            "The below visualizations explore crime data provided by the Boston Police Department
            for the month of April 2018. The first graph on the right plots the coordinates of all the crimes
            committed in Boston. The second graph shows clusters where the most crimes were committed. You can
            zoom in to get a more specific view of where the crime occurred. The third map is a heat map that 
            also visualizes how crime is distributed in Boston. For all three maps, you can filter by a specific
            type of crime."),
            
            box(
              title = "Map of Boston Crime",
              leafletOutput("map_2018"),
              uiOutput("type_of_crime")
            ),
            
            box(
              title = "Areas with Most Crime",
              leafletOutput("map2"),
              uiOutput("crime2")),
            
            box(
            title = "Heat Map of Boston Crime", width = 11, solidHeader = TRUE, status = "primary",
            tags$style('.leaflet {width: 600px; height: 400px;}'),
            tags$head(tags$script(src="http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js")),
            leafletOutput("map4"),
            selectInput(
              "heat",
              "Type of Crime:",
              choices = choices2
            ),
            
            sliderInput("opacity", "Opacity:",
                        min = 0, max = 1,
                        value = 0.25, step = 0.05),
            sliderInput("radius", "Radius:",
                        value = 5,
                        min = 0, max = 25
            ),
            sliderInput("blur", "Blur:",
                        min = 0, max = 30,
                        value = 0.75, step = 2)))),
   tabItem(tabName = "Graphs_2018",
          fluidRow(
            box( 
              dygraphOutput("graph2"),
              selectInput("crime",
                          "Type of Crime:",
                          choices = choices3
            )),
            box(
              plotlyOutput("graph4"),
              selectInput("hourly",
                          "Type of Crime:",
                          choices = choices3
            )
            ),
            box(
              plotlyOutput("graph6"),
              selectInput("weekly",
                          "Type of Crime:",
                          choices = choices2
                          )),
            box(align = 'right',
                height = 500,
                status = 'primary',
              plotOutput("graph5")
              
            )
            )
          ),
tabItem(tabName = "Crime_2015",
      fluidRow(
          column(12,
          box("The Boston police department started collecting data regularly beginning halfway through June of 2018. Therefore,
              I included information from this month since this is the first full montht that data was collected. The graph below
              visualizes how crime rates fluxuated throughout the month of July in 2015 and the map below outlines where the crimes took place.
              Both of these visualizations seek to give you brief comparison of crime in Boston in 2015 compared to 2018.")),
          box(
            leafletOutput("map_2015")),
          selectInput("dot_2015",
                      "Type of Crime:",
                      choices = choices_2015),
          box(
          plotOutput("graph7"))

)
)
)
)
)





server <- function(session, input, output) {
  
  output$map_2015 <- renderLeaflet ({
    gg2 <- leaflet() %>% addTiles() %>% 
      setView(-71.0616, 42.36, zoom = 13) 
  })
  
  observeEvent(input$dot_2015, {
    Crime = input$dot_2015
    ## supbset the data based on the choice
    if(Crime != 'All'){
      Boston_crime2 <- crime_2015_clean[crime_2015_clean$OFFENSE_CODE_GROUP == Crime, ]
    }else{
      Boston_crime2 <- crime_2015_clean
    }
    ## plot the subsetted data
    leafletProxy("map_2015") %>%
      clearMarkers() %>%
      addCircleMarkers(Boston_crime2$Long, Boston_crime2$Lat,
                       radius= 4, fillOpacity = 0.5, stroke = FALSE)
  })
  
  output$map_2018 <- renderLeaflet({
    gg <- leaflet() %>% addTiles() %>% 
      setView(-71.0616, 42.36, zoom = 13) 
  })
  
  output$type_of_crime <- renderUI({
    choices <- as.character(unique(Boston_crime_clean$OFFENSE_CODE_GROUP))
    choices <- c('All', choices)
    selectInput(
      "type_of_crime",
      "Type of Crime:",
      choices = choices
    )
  })
  
  observeEvent(input$type_of_crime, {
    Crime = input$type_of_crime
    ## supbset the data based on the choice
    if(Crime != 'All'){
      Boston_crime_clean_2 <- Boston_crime_clean[Boston_crime_clean$OFFENSE_CODE_GROUP == Crime, ]
    }else{
      Boston_crime_clean_2 <- Boston_crime_clean
    }
    ## plot the subsetted data
    leafletProxy("map_2018") %>%
      clearMarkers() %>%
      addCircleMarkers(Boston_crime_clean_2$Long, Boston_crime_clean_2$Lat,
                       radius= 4, fillOpacity = 0.5, stroke = FALSE)
  })
  
  output$map2 <- renderLeaflet({
    h <- leaflet() %>% addTiles() %>% 
      setView(-71.0616, 42.36, zoom = 11) 
  })
  
  output$crime2 <- renderUI({
    
    choices1 <- as.character(unique(Boston_crime_clean$OFFENSE_CODE_GROUP))
    choices1 <- c('All', choices1)
    selectInput(
      "crime2",
      "Type of Crime:",
      choices = choices1
    )
    
  })
  
  
  observeEvent(input$crime2, {
    
    most_crime = input$crime2
    
    ## supbset the data based on the choice
    if(most_crime != 'All'){
      Boston_most <- Boston_crime_clean[Boston_crime_clean$OFFENSE_CODE_GROUP == most_crime, ]
      
    }else{
      Boston_most <- Boston_crime_clean
    }
  
  
 # output$map2 <- renderLeaflet({
    leafletProxy("map2") %>%
    clearMarkerClusters %>%
    addCircleMarkers(data = Boston_most, lat = Boston_most$Lat, lng = Boston_most$Long, radius = 5, 
                     #color = ~ ifelse(OFFENSE_CODE_GROUP == input$crime2, 'red', 'blue'),
                     clusterOptions = markerClusterOptions())
  })
  
  xts_data <- reactive({
    # select  <- count %>% select(Date, Count, Crime_Type)
    # selData <- select[select$Crime_Type == input$crime, ]
    crime = input$crime
    
    if(crime != 'All'){
      select  <- count %>% select(Date, Count, Crime_Type)
      selData <- select[select$Crime_Type == input$crime, ]
      
    }else{
      select <- monthly_2018 %>% select(Date, Count)
    }
    
  })

  output$graph2 <- renderDygraph({
    xts_dat <- xts(xts_data(), xts_data()$Date)
    
   dygraph(xts_dat, main = "Daily Crime Totals for the month of April") %>%
      dyAxis("y", label = "Number of Crimes")
    
  })
  
  
  hourlyData <- reactive({
    selData  <- time_csv %>% filter(as.character(Crime_Type) == input$hourly) %>% select(-X, Time, Count)
  })
  
  output$graph4 <- renderPlotly({
    plot_ly(x = ~hourlyData()$Time, y = ~hourlyData()$Count, type = 'scatter', mode = 'lines') %>%
      layout(title = "Hourly Crime Totals",
             xaxis = list(title = "Hour of Day"),
             yaxis = list (title = "Number of Crimes"))
             
  })

  
  output$graph6 <- renderPlotly({
    crime = input$weekly
    
    if(crime != 'All'){
      selData <- weekly_crime %>% filter(as.character(Crime_Type) == input$weekly) %>% select(Day, Count)
      
    }else{
      selData <- All_totals
    }
    
     plot_ly(x = ~selData$Day, y = ~selData$Count, type = 'scatter', mode = 'lines') %>%
       layout(title = "Crime Totals for Each Day of the Week", 
             # plot_bgcolor='#4666d1', paper_bgcolor='#4666d1', 
              xaxis = list(title = "Day of Week", categoryorder = "array",categoryarray = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
              yaxis = list (title = "Number of Crimes"))
      })
      
  
   output$map4 <- renderLeaflet({
     leaflet() %>%
       addProviderTiles(providers$Stamen.TonerLite,options = providerTileOptions(noWrap = TRUE)) %>%
       setView(-71.0616, 42.36, zoom = 11)

   })
   
   observeEvent(input$heat, {
     
     Crime_3 = input$heat
     
     if(Crime_3 != 'All'){
       Bos_heat <- Boston_crime_clean[Boston_crime_clean$OFFENSE_CODE_GROUP == Crime_3, ]

     }else{
       Bos_heat <- Boston_crime_clean
     }
     leafletProxy("map4") %>%
      clearHeatmap() %>%
      addHeatmap(data = Bos_heat, lng = Bos_heat$Long, lat = Bos_heat$Lat, minOpacity= ~input$opacity, blur = ~input$blur, radius = ~input$radius)
  })
   
   output$graph5 <- renderPlot ({
     par(mar=c(4,12,1,1))
     barplot(top_20[,1], main = "Top 20 Crimes", cex.names=0.8,
              xlab = "Number of Crimes", names.arg = top_20$Crime_Type, horiz = T, las = 1)
   })
   
   output$graph7 <- renderPlot ({
     ggplot(monthly_2015, aes(x= Date, y = Count, group = 1)) +
       geom_line(color="red") +
       ggtitle("Number of Crimes throughout July 2015") +
       theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank()) 

   })

  
  }

  


shinyApp(ui=ui, server=server)