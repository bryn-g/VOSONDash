shinyServer(function(input, output, session) {
  gg <- new.env()
  
  source("server/srv_logger.R", local = TRUE)
  source("server/srv_data/srv_console.R", local = TRUE)
  source("server/srv_includes.R", local = TRUE)
  
  # on startup
  observeEvent(input$nav_sel_tab_id, {
    # clear consoles
    cons <- list("mtdn_console", "ytbe_console", "reddit_console", "hyperlink_console")
    for (i in seq_len(length(cons))) reset_console(cons[[i]], FALSE)
   
    # macos
    if (VOSONDash::is_macos()) shinyjs::enable("macos_font_check")
  }, once = TRUE, ignoreInit = FALSE)
  
  # stop app when browser closes
  session$onSessionEnded(function() {
    if (isLocal) {
      message("Session ended or browser closed. Exiting.\n")
      stopApp()
    }
  })
})
