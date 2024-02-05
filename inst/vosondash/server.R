shinyServer(function(input, output, session) {
  
  # auth
  u_api_keys_path <- "NA"
  u_api_tokens_path <- "NA"
  
  if (isLocal) {
    u_api_keys_path <- paste0(Sys.getenv("HOME"), "/vosondash_keys.rds", sep = "")
    u_api_tokens_path <- paste0(Sys.getenv("HOME"), "/vosondash_tokens.rds", sep = "")
  }
  
  source("server/srv_app_log.R", local = TRUE)
  source("server/srv_console.R", local = TRUE)
  
  source("server/srv_network_load.R", local = TRUE)
  source("server/srv_network_save.R", local = TRUE)
  source("server/srv_network_meta.R", local = TRUE)
  source("server/srv_controls.R", local = TRUE)
  
  source("server/srv_graph_base.R", local = TRUE)
  source("server/srv_graph_filtered.R", local = TRUE)

  source("server/srv_node_rm.R", local = TRUE)
  source("server/srv_graph_comps.R", local = TRUE)
  source("server/srv_graph_cats.R", local = TRUE)
  source("server/srv_node_dt.R", local = TRUE)
  source("server/srv_edge_dt.R", local = TRUE)
  source("server/srv_plot.R", local = TRUE)
  
  source("server/srv_network_text.R", local = TRUE)
  source("server/srv_network_metrics.R", local = TRUE)  
  source("server/srv_network_assort.R", local = TRUE)
  
  source("server/srv_collect_mtdn.R", local = TRUE)
  source("server/srv_collect_ytbe.R", local = TRUE)
  source("server/srv_collect_rddt.R", local = TRUE)
  source("server/srv_collect_web.R", local = TRUE)
  
  source("server/srv_auth.R", local = TRUE)
  
  
  if (VOSONDash::isMac()) shinyjs::enable("macos_font_check")
  
  # reset collect consoles on startup
  observeEvent(input$sidebar_menu, {
    cons <- list("mtdn_console", "ytbe_console", "reddit_console", "hyperlink_console")
    for (i in seq_len(length(cons))) reset_console(cons[[i]], FALSE)
  }, once = TRUE, ignoreInit = FALSE)
  
  # stop app when browser closes
  session$onSessionEnded(function() {
    if (isLocal) {
      message("Session ended or browser closed. Exiting.\n")
      stopApp()
    }
  })
})
