log_rv <- reactiveValues(log = c("initiating log."))

# server log function
dash_logger <- function(x, parent_env = FALSE) {
  if (!isTruthy(log_rv$log)) {
    cat(file=stderr(), paste0(x, "\n"))
  } else {
    if (parent_env) {
      log_rv$log <<- VOSONDash::logMessage(log_rv$log, x)
    } else {
      log_rv$log <- VOSONDash::logMessage(log_rv$log, x)
    }
  }
}

output$app_log_txt <- renderText({
  paste0(log_rv$log, collapse = "\n")
})

ts_utc <- function() {
  t <- Sys.time()
  attr(t, "tzone") <- "UTC"
  format(t, "%Y-%m-%d %H:%M:%S %Z")
}
