library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(plotly)

#Establecemos nuestro directorio de trabajo
getwd()
setwd('C:\\UPM\\BookingShinyApp')

df = read.csv('data\\hotel_bookings.csv')

#convert to factor (categorical)
df$hotel=as.factor(df$hotel)
df$is_canceled=as.factor(df$is_canceled)
df$arrival_date_month=factor(df$arrival_date_month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ))
df$arrival_date_day_of_month=as.factor(df$arrival_date_day_of_month)

#We create new quantitative variables
df$guests <- df$adults + df$children + df$babies
df$nights <- df$stays_in_weekend_nights + df$stays_in_week_nights

#Reservation with guests
df <- filter(df, guests > 0)
#Reservation with nights
df <- filter(df, nights > 0)

df.heatmap <- subset(df, select= c(hotel, arrival_date_year, arrival_date_month, arrival_date_day_of_month, is_canceled, guests, nights))
colnames(df.heatmap)=c("hotel","Year","Month","Day","is_canceled","guests","nights")

years <- unique(df$arrival_date_year)

  # Define UI ----
ui <- fluidPage(
    titlePanel("Hotel booking demand App"),
    sidebarLayout(
      sidebarPanel(
        h2("Hotel booking demand app"),
        p("The main goal of this app is to help an analyst to find some insights about the demand of hotels."),
        
        p("The analyst could be the regional manager of a major hotel chain that uses reservation information to make the best decisions regarding adjustments to its facilities, marketing strategies to attract more guests, and everything else needed to offer the best possible experience to its guests."),
        
        br(),
        br(),
        p('These visualizations aim to help the analyst to find aswers to three different questions:'),
        p(strong('1.- Demand distribution throughout the year')),
        selectInput("hotel", 
                    label = "Choose the type of hotel",
                    choices = c("City Hotel", "Resort Hotel", "All"),
                    selected = "All"),
        selectInput("year", 
                    label = "Choose a year to filter",
                    choices = c(years, "All"),
                    selected = "2016"),
        selectInput("cancelled", 
                    label = "Choose if you want to filter cancelled",
                    choices = c("Cancelled", "Not Cancelled", "All"),
                    selected = "Not Cancelled"),
        selectInput("score", 
                    label = "Choose the variable to measure",
                    choices = c("Guests", "Nights"),
                    selected = "Guests"),
        br(),
        br(),
        br("This app was developed by"),
        p(a("alvaroame", 
            href = "https://github.com/alvaroame")),
        br("Powered by"),
        "Shiny, a product of ", 
        span("RStudio", style = "color:blue")
      ),
      mainPanel(
        h1("Introducing Hotel booking demand dataset"),
        p("The dataset represents information regarding the demand for two types of hotels, one is a resort-type hotel and the other is an urban hotel, it contains 32 attributes that include information such as when the reservation was made, length of stay, number of adults , children and/or babies, the number of available parking spaces, among other things. and approximately 119,000 observations where each observation represents a hotel reservation from July 1, 2015 to August 31, 2017, including reservations that actually arrived and those that were cancelled."),
        
        p(a("Dataset in Kaggle", 
            href = "https://www.kaggle.com/jessemostipak/hotel-booking-demand")),
        br(),
        h2("How is demand distributed throughout the year?"),
        p("We plot a heat map"),
        br(),
        h4("Heatmap plot"),
        p("With this tool the analyst can explore the distribution over the year"),
        plotlyOutput("heatPlot"),
        p(),
        br(),
        
      )
    )
  )
  # Define server logic ----
  server <- function(input, output, session) {
    
    getHeatMapData <- reactive({
      #for hotel
      dataHeatMap <- switch(input$hotel, 
                            "City Hotel" = filter(df.heatmap, hotel == "City Hotel"),
                            "Resort Hotel" = filter(df.heatmap, hotel == "Resort Hotel"),
                            "All" = df.heatmap)
      
      #for one year
      dataHeatMap <- switch(input$year, 
                            "2015" = filter(dataHeatMap, Year == 2015),
                            "2016" = filter(dataHeatMap, Year == 2016),
                            "2017" = filter(dataHeatMap, Year == 2017),
                            "All" = dataHeatMap)
      
      #not cancelled
      dataHeatMap <- switch(input$cancelled, 
                            "Cancelled" = filter(dataHeatMap, is_canceled == 1),
                            "Not Cancelled" = filter(dataHeatMap, is_canceled == 0),
                            "All" = dataHeatMap)
      
      
      print(input$hotel)
      print(input$year)
      print(input$cancelled)
      print(input$score)
      
      #we sum-up totals
      df.heatmap.totals <- dataHeatMap %>% 
        group_by(Month, Day) %>% 
        summarise(guests = sum(guests), nights = sum(nights))
      
      #text for tooltip
      df.heatmap.totals <- df.heatmap.totals %>%
        mutate(text = paste0(Month, " ", Day, "\n",
                             "Guests: ", guests, "\n", 
                             "Nights: ", nights, "\n"))
      
      df.heatmap.totals
    })
    
    # heatmap 
    output$heatPlot <- renderPlotly({
      dataHeatMap <- getHeatMapData()
      #we plot the heatmap
      heatMapPlot <- ggplot(dataHeatMap, aes_string(x = 'Day', y = 'Month', fill = tolower(input$score), text='text')) +
        geom_tile() + 
        scale_fill_gradient(low = "white", high = "steelblue") +
        theme_light() 
      plotly <- ggplotly(heatMapPlot, tooltip="text")
    })
    
  }
  
  # Run the app ----
  shinyApp(ui = ui, server = server)