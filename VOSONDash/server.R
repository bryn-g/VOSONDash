# voson dashboard shiny app server

#### shiny server ----------------------------------------------------------------------------------------------------- #
shinyServer(function(input, output, session) {
  #### network graphs ####
  source("server/networkGraphsServer.R", local = TRUE)
  
  #### text analysis ####
  source("server/textAnalysisServer.R", local = TRUE)
  
  #### network metrics ####
  source("server/networkMetricsServer.R", local = TRUE)  
  
  #### assortativity ####
  source("server/assortativityServer.R", local = TRUE)
  
  #### twitter ####
  source("server/twitterServer.R", local = TRUE)
  
  #### youtube ####
  source("server/youtubeServer.R", local = TRUE)
  
  #### reddit ####
  source("server/redditServer.R", local = TRUE)
  
  #### api keys ####
  source("server/apiKeysServer.R", local = TRUE)
  
  output$userMenu <- renderMenu({
    from <- "from test"
    message <- "message test"
    messageData <- data.frame(from, message, stringsAsFactors = FALSE)
    
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  observeEvent(input$sidebar_menu, {
    resetConsole("twitter_console", FALSE)
    resetConsole("youtube_console", FALSE)
    resetConsole("reddit_console", FALSE)
  }, once = TRUE, ignoreInit = FALSE)
  
  # stop app when browser closes
  session$onSessionEnded(function() {
    if (isLocal) {
      cat("Session ended or browser closed. Exiting.\n")
      stopApp()
    }
  })  
}) #### end shinyServer
