# graphml file uploaded
observeEvent(input$graphml_data_file, {
  infile <- req(input$graphml_data_file)

  # reads file as graphml and fails gracefully
  tryCatch({
    g_rv$data <- igraph::read_graph(infile$datapath, format = "graphml")
    
    g_meta_rv$data <- list(
      desc = "Imported from file.",
      type = ifelse(
        "type" %in% graph_attr_names(g_rv$data), graph_attr(g_rv$data, "type"), "import"),
      name = infile$name,
      uploaded = ts_utc()
    )
    dash_logger(paste0("uploaded file: ", infile$name))
  }, error = function(err) {
    dash_logger(paste0("error uploading file: ", infile$name, "\n[", err, "]"))
  })
})

# find package data
f_get_pkg_data_files <- function() {
  f_list <- c()
  tryCatch({
    f_list <- list.files(
      path = system.file("extdata", "", package = "VOSONDash", mustWork = TRUE),
      pattern = "\\.graphml$"
    )
  }, error = function(err) {
    dash_logger(paste0("error finding package data sets. [", err, "]"))
  })
  
  if (length(f_list) < 1) return(NULL)
  
  f_names <- lapply(f_list, function(x) gsub("\\.graphml$", "", x, ignore.case = TRUE))
  names(f_list) <- f_names
  f_list
}

# enable package data selection
observeEvent(input$pkg_data_sel, {
  if (is.null(g_pkg_rv$files)) g_pkg_rv$files <- f_get_pkg_data_files()
  
  updateSelectInput(session, "pkg_data_sel", label = NULL, choices = g_pkg_rv$files, selected = NULL)
  shinyjs::enable("pkg_data_sel")
  shinyjs::enable("pkg_data_sel_btn")
}, ignoreInit = FALSE, once = TRUE)

# load package data file
observeEvent(input$pkg_data_sel_btn, {
  req(input$pkg_data_sel_btn, input$pkg_data_sel)
  
  data <- NULL
  tryCatch({
    f <- system.file("extdata", input$pkg_data_sel, package = "VOSONDash")
    data <- igraph::read_graph(f, format = c('graphml'))
  }, error = function(err) {
    dash_logger(paste0("error loading file: ", f, "\n[", err, "]"))
  })
  
  req(data)
  
  type <- ifelse("type" %in% igraph::graph_attr_names(data), igraph::graph_attr(data, "type"), "")
  desc <- "Description file not found."
  
  tryCatch({
    desc <- paste0(readLines(paste0(f, ".txt")), collapse = "<br>")
  }, error = function(err) {
    dash_logger(paste0("error loading description file: ", paste0(f, ".txt"), "\n[", err, "]"))
  })

  g_rv$data <- data
  g_meta_rv$data <-
    list(
      desc = desc,
      type = type,
      name = input$pkg_data_sel,
      uploaded = ts_utc()
    )
})
