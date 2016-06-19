library(httr)

rm(list=ls())


response_time_recording <- data.frame(timestamp = NULL,
                                      responsetime = NULL)

while(TRUE){
  
  url_to_monitor <- strsplit(scan("configuration", what="char", quiet = TRUE)[1], split = "@")[[1]][2]
  checking_interval <- as.numeric(strsplit(scan("configuration", what="char", quiet = TRUE)[2], split = "@")[[1]][2])
  
  
  check_timestamp <- as.character(Sys.time())
  check_result <- try(GET(url_to_monitor))
  
  if(class(check_result) != "try-error"){
    cat(class(check_result), file="current_check_result", append = FALSE)
    cat("\n", file="current_check_result", append = TRUE)
    cat(check_result$status_code, file="current_check_result", append = TRUE)
    cat("\n", file="current_check_result", append = TRUE)
    cat(check_result$times['total'], file="current_check_result", append = TRUE)
    cat("\n", file="current_check_result", append = TRUE)
    cat(url_to_monitor, file="current_check_result", append = TRUE)
    cat("\n", file="current_check_result", append = TRUE)
    cat(checking_interval, file="current_check_result", append = TRUE)
  } else {
    cat(class(check_result), file="current_check_result", append = FALSE)
    cat("\n", file="current_check_result", append = TRUE)
    cat(444, file="current_check_result", append = TRUE)
    cat("\n", file="current_check_result", append = TRUE)
    cat(20, file="current_check_result", append = TRUE)
    cat("\n", file="current_check_result", append = TRUE)
    cat(url_to_monitor, file="current_check_result", append = TRUE)
    cat("\n", file="current_check_result", append = TRUE)
    cat(checking_interval, file="current_check_result", append = TRUE)
  }

  
  if(class(check_result) == "try-error"){
    response_time <- 20
  } else {
    response_time <- unname(check_result$times["total"])
  }
  
  temp_result <- data.frame(timestamp = check_timestamp,
                            responsetime = response_time)
  
  
  response_time_recording <- rbind(response_time_recording, temp_result)
  write.csv(response_time_recording, file = "response_time_recording.csv")
  
  Sys.sleep(checking_interval)
  
  
}