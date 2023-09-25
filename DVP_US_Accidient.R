#=================================================================================
# Student Name: Yaoxing Chen
# Student ID: 32664443
# 
# Tip:
#   There is a lot of data in the database, 
#   and loading will consume a lot of time. 
#   Please wait a moment,thanks for your patience.
#================================================================================

library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(hrbrthemes)
library(shinythemes)
library(tidyverse)
library(stringr)
library(lubridate)
library(shiny)
library(shinycustomloader)
library(shinydashboard)


Maryland_CandV <- read_csv("Maryland_AccidentsAndViolation.csv")
US_Accidents_Dec21 <-
  read_csv("US_Accidient_Updated.csv")


US_Accidient_UpdatedTime <-
  US_Accidents_Dec21 %>% mutate(Start_Time_Year = year(Start_Time)) %>%
  mutate(Start_Time_Month = month(Start_Time)) %>% mutate(Start_Time_Day = day(Start_Time))

MonthCollecttion <-
  unique(US_Accidient_UpdatedTime$Start_Time_Month)
YearCollecttion <- unique(US_Accidient_UpdatedTime$Start_Time_Year)

NewDataCandV <-
  within(Maryland_CandV, DateANDTimePart1[is.na(DateANDTimePart1)] <-
           DateANDTimePart2[is.na(DateANDTimePart1)]) %>%
  select(TypeOfEvent, DateANDTimePart1, Latitude, Longitude) %>% mutate(Year = year(DateANDTimePart1)) %>% mutate(Month = month(DateANDTimePart1)) %>%
  mutate(Day = day(DateANDTimePart1)) %>% mutate(Hour = hour(DateANDTimePart1))

MonthCollecttionMaryland <- unique(NewDataCandV$Month)
YearCollecttionMaryland <- unique(NewDataCandV$Year)


ui <- fluidPage(
  # Visualization title
  titlePanel(h1("US CAR ACCIDENTS AND VIOLATION DATA VISUALIZATION PROJECT", align = "center")),
  
  # Sidebar with a selected input for several charts
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Select_Year_US",
        label = "Select an certain year for US:",
        choices = c(YearCollecttion),
        selected = "2021"
      ),
      selectInput(
        inputId = "Select_Year_Maryland",
        label = "Select an certain year for Maryland:",
        choices = c(YearCollecttionMaryland),
        selected = "2016"
      ),
      selectInput(
        inputId = "Select_Month",
        label = "Select an certain month:",
        choices = c(MonthCollecttion),
        selected = "9"
      ),
      br(),
      h4("Outline of visualization:"),
      h5(
        "When we are driving on the road, the weather, road conditions all affect the driving safety factor."
      ),
      h5(
        "The dataset of the United States, which ranks among the top car ownership per capita, as a data source to ensure that the results of the project have some significance."
      ),
      h5(
        "In this data visualization project, I will show the difference in the number of traffic accidents by state in the US, and the impact of humidity, pressure, temperature, and visibility on the amount of accidents"
      ),
      h5(
        "In order to ensure that the data analysis results are not disturbed by the gap in the amount of data. I re-collected traffic accident and violation data in the state of Maryland – that is the third dataset"
      ),
      h5("TIPS! :", style="color:red;"),
      h5("1. There is a lot of data in the database, and loading will consume a lot of time. Thanks for your patience!"),
      h5("2. If there is no relevant visualization in the chart, it means that there is no relevant data under the time you just set."),
      br(),
      h4("Datasets(original):"),
      h5("US Accidents, Maryland Accidents, Maryland Violation"),
      a(
        "https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents"
      ),
      a(
        "https://data.world/jrm/trafficviolations/workspace/file?filename=Traffic_Violations.csv"
      ),
      a(
        "https://catalog.data.gov/dataset/crash-reporting-incidents-data"
      )
    ),
    
    mainPanel(
      h4(
        "Indicate the relationship between the amount of accidient and State, you can choose another year at left top panel."
      ),
      plotlyOutput("Chart1"),
      br(),
      h4(
        "Finding which meteorological conditions has the most dramatic impact on the amount of accidents"
      ),
      h4("This is for Humidity:"),
      br(),
      plotlyOutput("HumidityChart"),
      br(),
      h4("This is for Air Pressure:"),
      plotlyOutput("PressureChart"),
      br(),
      h4("This is for Temperature:"),
      plotOutput("TemperatureChart"),
      br(),
      h4("This is for Visibility:"),
      plotOutput("VisibilityChart"),
      br(),
      h4("This is for Weather Condition:"),
      plotOutput("WeatherConChart"),
      br(),
      h4("This is for MaryLand"),
      plotOutput("EventsAndDay")
    )
  )
)


server <- function(input, output) {
  US_selectedTimeCount <- reactive({
    US_Accidient_UpdatedTime %>%
      select(ID, Start_Time_Year, City, County, State) %>%
      filter(Start_Time_Year == input$Select_Year_US) %>%
      group_by(State) %>%
      count("Accidient_Count_By_State")
  })
  
  output$Chart1 <- renderPlotly({
    ggplot(US_selectedTimeCount()) +
      aes(x = State, y = n) +
      geom_col(fill = "#D69648") +
      labs(
        x = "The name of state",
        y = "Amount",
        title = "Amount of accidient by selected year",
        caption = "Tips: You can select the domain of year by moving slider"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          size = 24L,
          face = "bold",
          hjust = 0.5
        ),
        plot.caption = element_text(size = 12L),
        axis.title.y = element_text(size = 14L,
                                    face = "bold"),
        axis.title.x = element_text(size = 16L,
                                    face = "bold")
      )
  })
  
  US_selectedTime_By_Humidity <-
    reactive({
      US_Accidient_UpdatedTime %>% filter(
        Start_Time_Year == input$Select_Year_US &
          Start_Time_Month == input$Select_Month
      ) %>%
        select(`Humidity(%)`,
               `Temperature(F)`,
               `Pressure(in)`,
               `Visibility(mi)`) %>%
        group_by(`Humidity(%)`) %>%
        count("Accidient_By_H")
    })
  
  US_selectedTime_By_Temperature <-
    reactive({
      US_Accidient_UpdatedTime %>% filter(
        Start_Time_Year == input$Select_Year_US &
          Start_Time_Month == input$Select_Month
      ) %>%
        select(`Humidity(%)`,
               `Temperature(F)`,
               `Pressure(in)`,
               `Visibility(mi)`) %>%
        group_by(`Temperature(F)`) %>%
        count("Accidient_By_T")
    })
  
  US_selectedTime_By_Pressure <-
    reactive({
      US_Accidient_UpdatedTime %>% filter(
        Start_Time_Year == input$Select_Year_US &
          Start_Time_Month == input$Select_Month
      ) %>%
        select(`Humidity(%)`,
               `Temperature(F)`,
               `Pressure(in)`,
               `Visibility(mi)`) %>%
        group_by(`Pressure(in)`) %>%
        count("Accidient_By_P")
    })
  
  US_selectedTime_By_Visibility <-
    reactive({
      US_Accidient_UpdatedTime %>% filter(
        Start_Time_Year == input$Select_Year_US &
          Start_Time_Month == input$Select_Month
      ) %>%
        select(`Humidity(%)`,
               `Temperature(F)`,
               `Pressure(in)`,
               `Visibility(mi)`) %>%
        group_by(`Visibility(mi)`) %>%
        count("Accidient_By_V")
    })
  
  output$HumidityChart <- renderPlotly({
    ggplot(US_selectedTime_By_Humidity()) +
      aes(x = `Humidity(%)`, y = n) +
      geom_line(size = 0.5, colour = "#FF8C00") +
      labs(x = "Humidity(%)",
           y = "Amount of accidient",
           title = "Line Chart of Accidents and Humidity ") +
      theme_linedraw() +
      theme(
        plot.title = element_text(
          size = 21L,
          face = "bold",
          hjust = 0.5
        ),
        axis.title.y = element_text(size = 16L,
                                    face = "bold"),
        axis.title.x = element_text(size = 16L,
                                    face = "bold")
      )
    
    
  })
  
  output$PressureChart <- renderPlotly({
    ggplot(US_selectedTime_By_Pressure()) +
      aes(x = `Pressure(in)`, y = n) +
      geom_line(size = 0.5, colour = "#0174AD") +
      labs(x = "Pressure(in)",
           y = "Amount of accident",
           title = "Line Chart of Accidents and Pressure") +
      theme_linedraw() +
      theme(
        plot.title = element_text(
          size = 20L,
          face = "bold",
          hjust = 0.5
        ),
        axis.title.y = element_text(size = 16L,
                                    face = "bold"),
        axis.title.x = element_text(size = 16L,
                                    face = "bold")
      ) +
      xlim(28, 31)
  })
  
  
  output$TemperatureChart <- renderPlot({
    ggplot(US_selectedTime_By_Temperature()) +
      aes(x = `Temperature(F)`, y = n) +
      geom_step(size = 0.5, colour = "#01A146") +
      labs(x = "Temperature(F)",
           y = "Amount of accident",
           title = "Line Chart of Accidents and Temperature") +
      theme_linedraw() +
      theme(
        plot.title = element_text(
          size = 20L,
          face = "bold",
          hjust = 0.5
        ),
        axis.title.y = element_text(size = 16L,
                                    face = "bold"),
        axis.title.x = element_text(size = 16L)
      ) +
      xlim(15, 105)
    
    
  })
  
  
  output$VisibilityChart <- renderPlot({
    ggplot(US_selectedTime_By_Visibility()) +
      aes(x = `Visibility(mi)`, y = n) +
      geom_line(size = 0.5, colour = "#007BE1") +
      labs(x = "Visibility(mi)",
           y = "Amount of accident",
           title = "Line Chart of Accidents and Visibility") +
      theme_minimal() +
      theme(
        plot.title = element_text(
          size = 20L,
          face = "bold",
          hjust = 0.5
        ),
        axis.title.y = element_text(size = 16L),
        axis.title.x = element_text(size = 16L,
                                    face = "bold")
      ) +
      xlim(3, 15)
    
  })
  
  
  US_selectedTime_By_WeatherCon <-
    reactive({
      US_Accidient_UpdatedTime %>% filter(
        Start_Time_Year == input$Select_Year_US &
          Start_Time_Month == input$Select_Month
      ) %>%
        select(ID, Weather_Condition) %>%
        group_by(Weather_Condition) %>%
        count("Accidient_By_WC") %>% summarise(TotalWeaCon = sum(n)) %>%
        arrange(desc(TotalWeaCon)) %>% top_n(20)
    })
  
  
  output$WeatherConChart <-
    renderPlot({
      ggplot(US_selectedTime_By_WeatherCon(),
             aes(x = Weather_Condition, y = TotalWeaCon)) +
        geom_bar(
          stat = "identity",
          position = position_dodge(width = 0.5),
          fill = rainbow(n=length(US_selectedTime_By_WeatherCon()$Weather_Condition))
        ) +
        theme_linedraw() +
        labs(x = "Weather Condition", y = "Total amount", title = "Total number of accident for each weather condition") +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 20L,
                                    face = "bold",
                                    hjust = 0.5),
          axis.title.x = element_text(size = 16L,
                                     face = "bold"),
          axis.text.x = element_text(angle = 90),
          axis.title.y = element_text(size = 16L,
                                      face = "bold"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")
        )
      
    })
  
  
  NewDataCandVselected <-
    reactive({
      NewDataCandV %>% filter(Year == input$Select_Year_Maryland &
                                Month == input$Select_Month) %>% group_by(Day, TypeOfEvent) %>% count("TotalAmount")
    })
  
  output$EventsAndDay <- renderPlot({
    ggplot(NewDataCandVselected()) +
      aes(x = Day, y = n, colour = TypeOfEvent) +
      geom_line(size = 1.5) +
      scale_color_brewer(palette = "Accent", direction = 1) +
      labs(x = "Day",
           y = "Amount of Events",
           title = "Line Chart: Events and Day") +
      theme_linedraw() +
      theme(
        plot.title = element_text(
          size = 20L,
          face = "bold",
          hjust = 0.5
        ),
        axis.title.y = element_text(size = 16L,
                                    face = "bold"),
        axis.title.x = element_text(size = 16L,
                                    face = "bold")
      )
  })

  
}



# Run the application
shinyApp(ui, server)











