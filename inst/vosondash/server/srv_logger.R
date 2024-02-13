log_rv <- reactiveValues(log = c("initiating log."))

# server log function
dash_logger <- function(..., parent_env = FALSE) {
  dots <- list(...)
  if (!isTruthy(log_rv$log)) {
    cat(file=stderr(), paste0(paste0(dots, collapse = ","), "\n"))
  } else {
    if (parent_env) {
      log_rv$log <<- VOSONDash::log_queue(log_rv$log, dots)
    } else {
      log_rv$log <- VOSONDash::log_queue(log_rv$log, dots)
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

dbg <- function(...) {
  dots <- list(...)
  cat(file=stderr(), paste0(paste0(dots, collapse = ","), "\n"))
}
