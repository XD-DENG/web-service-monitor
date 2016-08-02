library(shiny)
library(httr)
library(ECharts2Shiny)

# Prepare sample data for plotting --------------------------


check_interval <- 5
url_to_monitor <- "http://119.81.97.218/"

# Server function -------------------------------------------
server <- function(input, output, sessin) {

  dat <- data.frame(0)
  envir_of_dat <- environment()
  names(dat) <- c("Response.Time")

  monitor_log_writer <- observe({

    check_time <- Sys.time()
    check_result <- try(GET(url_to_monitor))
    
    if(class(check_result) != "try-error"){
      temp_respond_time <- check_result$times['total']
    } else {
      temp_respond_time <- 30
    }
    
    temp <- data.frame(c(temp_respond_time))
    names(temp) <- "Response.Time"
    row.names(temp) <- check_time

    assign('dat',  rbind(dat, temp), envir = envir_of_dat)

    streaming_dat <- dat
    if(dim(streaming_dat)[1] > 100){
      streaming_dat <- tail(streaming_dat, 100)
    }
    # re-plot
    renderLineChart(div_id = "streaming_sparkline", animation = FALSE, show.legend = FALSE,
                    data = streaming_dat, line.width = 1, font.size.axis.x = 10, rotate.axis.x = 45)

    renderLineChart(div_id = "historical_sparkline", animation = FALSE, show.legend = FALSE, theme = 'vintage',
                    data = dat, line.width = 1, font.size.axis.x = 10, rotate.axis.x = 45)



    invalidateLater(1000*check_interval, session=getDefaultReactiveDomain())
  })




}

# UI layout -------------------------------------------------
ui <- fluidPage(
  # We MUST load the ECharts javascript library in advance
  loadEChartsLibrary(),
  loadEChartsTheme('vintage'),

  tags$div(id="streaming_sparkline", style="width:100%;height:400px;"),
  deliverChart(div_id = "streaming_sparkline"),

  tags$div(id="historical_sparkline", style="width:100%;height:400px;"),
  deliverChart(div_id = "historical_sparkline")
)

# Run the application --------------------------------------
shinyApp(ui = ui, server = server)
