library(shiny)
library(shinydashboard)
library(httr)
library(plotly)



ui <- dashboardPage(
  dashboardHeader(title = "Web Service Monitor"),
  
  dashboardSidebar(disable = TRUE),
  
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
             )
    ),
    
    fluidRow(
      
      # Dynamic valueBoxes
      infoBoxOutput("url_working_or_not"),
      infoBoxOutput("respond_time")
    ),
    
    plotlyOutput("trendPlot")

  )
)









server <- function(input, output, session) {
  
  current_check_result <- reactiveFileReader(1000,
                                             session,
                                             "current_check_result",
                                             readLines)

  
  output$url_monitoring <- renderText({
    current_check_result()[4]
  })
  
  output$checking_interval <- renderText({
    paste(current_check_result()[5],
          "seconds")
  })
  
  
  

  
  output$url_working_or_not <- renderInfoBox({
    if(current_check_result()[1] == "try-error"){
      infoBox(
        "Service Availability",
        div("Service NOT Available", style="color:red"),
        icon = icon("thumbs-down", lib = "glyphicon"),
        color = "red"
      )
    } else {
      if(current_check_result()[2] == 200){
        infoBox(
          "Service Availability",
          div("Healthy",  style = "color:green"),
          icon = icon("thumbs-up", lib = "glyphicon"),
          color = "green"
        )
      } else {
        infoBox(
          "Service Availability",
          div("Not Found",  style = "color:blue"),
          icon = icon("thumbs-down", lib = "glyphicon"),
          color = "orange"
        )
      }

    }

  })
  
  
  output$respond_time <- renderInfoBox({
    if(current_check_result()[1] == "try-error"){
      infoBox(
        "response time (second)",
        "No Response",
        icon = icon("thumbs-down", lib = "glyphicon"),
        color = "red"
      )

    } else {
      if(current_check_result()[2] == 200){
        infoBox(
          "response time (second)",
          current_check_result()[3],
          color = "green"
        )
      }else{
        infoBox(
          "response time (second)",
          current_check_result()[3],
          color = "orange"
        )
      }
    }
  })
  
  
  trend_plot_dat <- reactiveFileReader(1000,
                                       session,
                                       "response_time_recording.csv",
                                       read.csv)
  
  output$trendPlot <- renderPlotly({
    dat <- trend_plot_dat()
    p <- plot_ly(dat, x = timestamp, y = responsetime)
    layout(p, title="Response Time Trend",
           xaxis = list(title = "Timestamp",  autorange = T),
           yaxis = list(title = "Response Time (second)",  autorange = T))
  })
  

}



shinyApp(ui, server)
