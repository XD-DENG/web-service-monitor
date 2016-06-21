library(shiny)
library(shinydashboard)
library(httr)
library(plotly)



response_time_recording <- data.frame(timestamp = NULL,
                                      responsetime = NULL)


shinyServer(function(input, output, session) {
  
  
  # Create a random name for the log file
  logfilename <- paste0('logfile',
                        floor(runif(1, 1e+05, 1e+06 - 1)),
                        ".csv")
  
  current_check_result_log <- paste0('current_check_result',
                                     floor(runif(1, 1e+05, 1e+06 - 1)),
                                     ".txt")
  
  # an observer to send GET the the url and get the response
  
  monitor_log_writer <- observe({
    
    check_interval <- as.numeric(strsplit(scan(file = "configuration", what="char", quiet = TRUE)[2], split = "@")[[1]][2])
    
    
    # Clear the log file if it exist
    if(file.exists(logfilename)){
      unlink(logfilename)
    }
    if(file.exists(current_check_result_log)){
      unlink(current_check_result_log)
    }
    
    
    
    url_to_monitor <- strsplit(scan("configuration", what="char", quiet = TRUE)[1], split = "@")[[1]][2]
    checking_interval <- as.numeric(strsplit(scan("configuration", what="char", quiet = TRUE)[2], split = "@")[[1]][2])
    
    check_timestamp <- as.character(Sys.time())
    check_result <- try(GET(url_to_monitor))
    
    if(class(check_result) != "try-error"){
      cat(class(check_result), file=current_check_result_log, append = FALSE)
      cat("\n", file=current_check_result_log, append = TRUE)
      cat(check_result$status_code, file=current_check_result_log, append = TRUE)
      cat("\n", file=current_check_result_log, append = TRUE)
      cat(check_result$times['total'], file=current_check_result_log, append = TRUE)
      cat("\n", file=current_check_result_log, append = TRUE)
    } else {
      cat(class(check_result), file=current_check_result_log, append = FALSE)
      cat("\n", file=current_check_result_log, append = TRUE)
      cat(444, file=current_check_result_log, append = TRUE)
      cat("\n", file=current_check_result_log, append = TRUE)
      cat(20, file=current_check_result_log, append = TRUE)
      cat("\n", file=current_check_result_log, append = TRUE)
    }
    
    
    if(class(check_result) == "try-error"){
      response_time <- 20
    } else {
      response_time <- unname(check_result$times["total"])
    }
    
    temp_result <- data.frame(timestamp = check_timestamp,
                              responsetime = response_time)
    
    if(is.na(url_to_monitor) == FALSE){
      response_time_recording <<- rbind(response_time_recording, temp_result)
    }
    write.csv(response_time_recording, file = logfilename)
    
    invalidateLater(1000*check_interval, session)
    
  })
  
  # when the sessions ends, deal with the observer properly
  session$onSessionEnded(function(){
    monitor_log_writer$suspend()
    unlink(logfilename)
    unlink(current_check_result_log)
  })
  
  
  
  current_check_result <- reactiveFileReader(100,
                                             session,
                                             current_check_result_log,
                                             readLines)
  
  
  
  live_configuration <- reactiveFileReader(100,
                                           session,
                                           "configuration",
                                           readLines)
  
  
  output$url_monitoring <- renderText({
    strsplit(live_configuration()[1], split = "@")[[1]][2]
  })
  
  output$checking_interval <- renderText({
    paste(strsplit(live_configuration()[2], split = "@")[[1]][2],
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
          round(as.numeric(current_check_result()[3]), 3),
          color = "green"
        )
      }else{
        infoBox(
          "response time (second)",
          round(as.numeric(current_check_result()[3]), 3),
          color = "orange"
        )
      }
    }
  })
  
  
  
  
  
  refresh_setting_action <- eventReactive(input$refresh_setting, {
    cat(paste("url_to_monitor@",
              input$url_to_monitor,
              "\n",
              "check_interval(seconds)@",
              input$check_interval,
              "\n",
              sep=""),
        file="configuration")
    
    
    paste("Setting Refreshed at:", as.character(Sys.time()))
  })
  
  output$refresh_setting_result <- renderText({
    refresh_setting_action()
  })
  
  refresh_data_action <- eventReactive(input$refresh_data, {
    
    # Refresh the "response recording" data frame so that the logging can restart
    response_time_recording <<- data.frame(timestamp = NULL,
                                           responsetime = NULL)
    
    paste("Data cleared at:", as.character(Sys.time()))
  })
  
  output$refresh_data_result <- renderText({
    refresh_data_action()
  })
  
  
  
  
  
  
  
  trend_plot_dat <- reactiveFileReader(1000,
                                       session,
                                       logfilename,
                                       read.csv)
  
  output$trendPlot <- renderPlotly({
    dat <- try(trend_plot_dat())
    if(class(dat) == "try-error"){
      return(NULL)
    }
    
    
    p <- subplot(
      p_1 <- plot_ly(tail(dat, 100), # only display the newest 100 results. This is somehow a "streaming"
                     x = timestamp, 
                     y = responsetime,
                     name = "Streaming (last 100 requests)"),
      
      p_2 <- plot_ly(dat,
                     x = timestamp, 
                     y = responsetime, 
                     name="Historical (since start)"),
      margin = 0.05,
      nrows=2
    ) %>% layout(showlegend = TRUE)
    p
  })
  
  
  
  
  output$mean_past_100 <- renderInfoBox({
    infoBox(
      "Average response time (second)",
      round(mean(tail(trend_plot_dat(), 100)$responsetime), 3),
      color = "blue"
    )
  })
  
  output$max_past_100 <- renderInfoBox({
    infoBox(
      "Max response time (second)",
      round(max(tail(trend_plot_dat(), 100)$responsetime), 3),
      color = "blue"
    )
  })
  
  output$min_past_100 <- renderInfoBox({
    infoBox(
      "Min response time (second)",
      round(min(tail(trend_plot_dat(), 100)$responsetime), 3),
      color = "blue"
    )
  })
  
  output$play_alarm_sound <- renderUI({
    if(current_check_result()[1] == "try-error" | current_check_result()[2] != 200)
    tags$audio(src = "alarm.mp3", type = "audio/mp3", autoplay = NA, controls = NA, style="display:none;")
  })
  

}
)
