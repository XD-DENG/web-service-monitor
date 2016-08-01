library(shiny)
library(httr)
library(ECharts2Shiny)

# Prepare sample data for plotting --------------------------


check_interval <- 5

# Server function -------------------------------------------
server <- function(input, output, sessin) {

  dat <- data.frame(0)
  names(dat) <- c("Response.Time")

  monitor_log_writer <- observe({

    temp <- data.frame(c(runif(1)))
    names(temp) <- c("Response.Time")
    row.names(temp) <- c(Sys.time())

    dat <<- rbind(dat, temp)
    if(dim(dat)[1]>20){
      dat<<-tail(dat, 20)
    }
    invalidateLater(1000*check_interval, session=getDefaultReactiveDomain())
    renderLineChart(div_id = "test", animation = FALSE,
                    data = dat, line.width = 1, font.size.axis.x = 8, rotate.axis.x = 45)
  })




}

# UI layout -------------------------------------------------
ui <- fluidPage(
  # We MUST load the ECharts javascript library in advance
  loadEChartsLibrary(),

  tags$div(id="test", style="width:100%;height:400px;"),
  deliverChart(div_id = "test")
)

# Run the application --------------------------------------
shinyApp(ui = ui, server = server)
