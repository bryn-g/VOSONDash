log_rv <- reactiveValues(log = c())

# log_rv$log <- logMessage(log_rv$log, "message")
# log_rv$log <<- logMessage(log_rv$log, "message")

output$app_log_txt <- renderText(paste0(log_rv$log, collapse = '\n'))

ts_utc <- function() {
  t <- Sys.time()
  attr(t, "tzone") <- "UTC"
  t
}
