library(shiny)
library(shinydashboard)
library(httr)
library(plotly)


if(file.exists("response_time_recording.csv")){
  file.remove("response_time_recording.csv")
}

response_time_recording <- data.frame(timestamp = NULL,
                                      responsetime = NULL)



shinyUI(dashboardPage(
  
  dashboardHeader(title = "Web Service Monitor"),
  
  dashboardSidebar(
    br(),		
    br(),		
    textInput("url_to_monitor", "Enter the URL to Monitor:",
              value = strsplit(scan("configuration", what="char")[1], split = "@")[[1]][2]),
    br(),
    numericInput("check_interval", "Checking Interval (second)",
                 value = 20, min = 10, step = 10),
    br(),
    actionButton("refresh_setting", "Refresh Setting"),
    br(),
    br(),
    actionButton("refresh_data", "Clear Data")
  ),
  
  dashboardBody(
    
    fluidRow(
      box(
        title = "URL Monitoring", 
        status = "primary", 
        strong(textOutput("url_monitoring")), 
        height = 80,
        width = 4
      ),
      box(
        title = "Check Interval",
        status = "primary",
        strong(textOutput("checking_interval")),
        height = 80,
        width = 3
      ),
      # help the refresh button work
      textOutput("refresh_setting_result"),
      textOutput("refresh_data_result")
    ),
    
    fluidRow(
      
      # Dynamic valueBoxes
      infoBoxOutput("url_working_or_not"),
      infoBoxOutput("respond_time")
    ),
    
    h3("During the Last 100 Requests"),
    
    fluidRow(
      infoBoxOutput("mean_past_100"),
      infoBoxOutput("max_past_100"),
      infoBoxOutput("min_past_100")
    ),
    
    fluidRow(
      column(1,br()),
      column(10,
             plotlyOutput("trendPlot", height = 600)
      ),
      column(1,br())
    ),
    
    uiOutput("play_alarm_sound")
    
    
    
  )
))


