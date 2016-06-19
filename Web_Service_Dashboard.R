library(shiny)
library(shinydashboard)
library(httr)



ui <- dashboardPage(
  dashboardHeader(title = "Web Service Monitor"),
  
  dashboardSidebar(
    br(),
    br(),
    textInput("url_to_monitor", "Enter the URL to Monitor:")
  ),
  
  dashboardBody(
    
    fluidRow(
             box(
               title = "URL Monitoring", 
               status = "primary", 
               strong(textOutput("url_monitoring")), 
               height = 80,
               width = 4
             )
             # box(
             #   title = "Check Interval", 
             #   status = "primary", 
             #   strong(textOutput("checking_interval")), 
             #   height = 80,
             #   width = 3
             # )
    ),
    
    fluidRow(
      
      # Dynamic valueBoxes
      infoBoxOutput("url_working_or_not"),
      infoBoxOutput("respond_time")
    )

  )
)









server <- function(input, output, session) {
  
  output$url_monitoring <- renderText({
    input$url_to_monitor
  })
  
  output$checking_interval <- renderText({
    input$checking_interval
  })
  
  
  check_result <- reactive({
    try(GET(input$url_to_monitor))
  })
  

  
  output$url_working_or_not <- renderInfoBox({
    if(class(check_result()) == "try-error"){
      infoBox(
        "Service Availability",
        div("Service NOT Available", style="color:red"),
        icon = icon("thumbs-down", lib = "glyphicon"),
        color = "red"
      )
    } else {
      if(check_result()$status_code == 200){
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
    if(class(check_result()) == "try-error"){
      infoBox(
        "response time (second)",
        "No Response",
        icon = icon("thumbs-down", lib = "glyphicon"),
        color = "red"
      )

    } else {
      if(check_result()$status_code == 200){
        infoBox(
          "response time (second)",
          check_result()$times["total"],
          color = "green"
        )
      }else{
        infoBox(
          "response time (second)",
          check_result()$times["total"],
          color = "orange"
        )
      }
    }
  })
  

}



shinyApp(ui, server)
