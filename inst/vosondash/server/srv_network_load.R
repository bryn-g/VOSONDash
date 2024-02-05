
# # graphml file uploaded
# observeEvent(input$graphml_data_file, {
#   r_set_graph_file()
#   
#   # set a random number to seed plots
#   graph_rv$graph_seed <- get_random_seed()
#   
#   # reset controls and filters
#   #setGraphTabControls()
#   # f_reset_filter_ctrls()
# })

# create list of demo files found in extdata - do once at startup
check_demo_files <- TRUE
observeEvent(check_demo_files, {
  tryCatch({
    demo_files_list <- list.files(path = system.file("extdata", "", package = "VOSONDash", mustWork = TRUE),
                                  pattern = "\\.graphml$")
    
    if (length(demo_files_list) > 0) {
      demo_files_list <- lapply(demo_files_list, function(x) gsub("\\.graphml$", "", x, ignore.case = TRUE))
      updateSelectInput(session, "demo_data_select", label = NULL, choices = demo_files_list,
                        selected = NULL)
      shinyjs::enable("demo_data_select")
      shinyjs::enable("demo_data_select_button")
    }    
  }, error = function(err) {
    log_rv$log <<- logMessage(log_rv$log, paste("error loading demo files:", err, "\n"))
  }, warning = function(w) {
    log_rv$log <<- logMessage(log_rv$log, paste("warning loading demo files:", w, "\n"))
  })
}, once = TRUE)

# load demo data button event
observeEvent(input$demo_data_select_button, {
  load_file <- system.file("extdata", paste0(input$demo_data_select, ".graphml"), package = "VOSONDash")
  
  if (load_file != "") {
    file_desc <- "Description file not found."
    tryCatch({
      file_desc <- paste(readLines(paste0(load_file, ".txt")), collapse = "<br>")
    }, error = function(err) {
      log_rv$log <<- logMessage(log_rv$log, paste("error loading demo files:", err, "\n"))
    }, warning = function(w) {
      log_rv$log <<- logMessage(log_rv$log, paste("warning loading demo files:", w, "\n"))
    })
    
    tryCatch({
      data <- igraph::read_graph(load_file, format = c('graphml'))
      type <- ifelse("type" %in% igraph::graph_attr_names(data), igraph::graph_attr(data, "type"), "")
    }, error = function(err) {
      log_rv$log <<- logMessage(log_rv$log, paste("error loading demo files:", err, "\n"))
    }, warning = function(w) {
      log_rv$log <<- logMessage(log_rv$log, paste("warning loading demo files:", w, "\n"))
    })
    
    req(data)
    
    graph_rv$data <- list(
      data = data,
      meta = list(
        desc = file_desc,
        type = type,
        subtype = mtdn_collect_rv$param_type,
        network = mtdn_rv$network_type,
        name = input$demo_data_select,
        uploaded = ts_utc()
      )
    )
  }
})

# # set file data when a file is uploaded
# r_set_graph_file <- reactive({
#   req(input$graphml_data_file)
#   
#   infile <- input$graphml_data_file
#   
#   # if (is.null(infile)) { return(NULL) }
#   
#   # reads file as graphml and fails gracefully
#   tryCatch({
#     g <- igraph::read_graph(infile$datapath, format = c('graphml'))
#     graph_rv$graph_name <- infile$name
#   }, error = function(err) { return(NULL) })
#   
#   graph_rv$data <- g
#   graph_rv$graph_type <- ifelse(
#     "type" %in% igraph::graph_attr_names(g), 
#     igraph::graph_attr(g, "type"),
#     ""
#   )
#   
#   graph_rv$meta$desc <- "Network loaded from file."
#   
#   # addNodeContinuous()
#   createGraphCategoryList()
#   
#   updateCheckboxInput(session, "expand_demo_data_check", value = FALSE)
# })
