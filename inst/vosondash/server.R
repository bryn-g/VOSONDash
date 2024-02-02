# voson dashboard shiny app server

source("server/srv_utils.R")

#### shiny server ----------------------------------------------------------------------------------------------------- #
shinyServer(function(input, output, session) {
  
  # auth
  u_api_keys_path <- "NA"
  u_api_tokens_path <- "NA"
  
  if (isLocal) {
    u_api_keys_path <- paste0(Sys.getenv("HOME"), "/vosondash_keys.rds", sep = "")
    u_api_tokens_path <- paste0(Sys.getenv("HOME"), "/vosondash_tokens.rds", sep = "")
  }
  
  source("server/srv_analysis_network.R", local = TRUE)
  source("server/srv_analysis_text.R", local = TRUE)
  source("server/srv_network_metrics.R", local = TRUE)  
  source("server/srv_network_assortativity.R", local = TRUE)
  source("server/srv_collect_mtdn.R", local = TRUE)
  source("server/srv_collect_ytbe.R", local = TRUE)
  source("server/srv_collect_rddt.R", local = TRUE)
  source("server/srv_collect_web.R", local = TRUE)
  source("server/srv_auth.R", local = TRUE)
  source("server/srv_console.R", local = TRUE)
  
  if (VOSONDash::isMac()) shinyjs::enable("macos_font_check")
  
  # reset collect consoles on startup
  observeEvent(input$sidebar_menu, {
    resetConsole("mtdn_console", FALSE)
    resetConsole("ytbe_console", FALSE)
    resetConsole("reddit_console", FALSE)
    resetConsole("hyperlink_console", FALSE)
  }, once = TRUE, ignoreInit = FALSE)
  
  # stop app when browser closes
  session$onSessionEnded(function() {
    if (isLocal) {
      message("Session ended or browser closed. Exiting.\n")
      stopApp()
    }
  })
}) #### end shinyServer
