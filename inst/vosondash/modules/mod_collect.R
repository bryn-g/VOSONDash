collect_data_btns_ui <- function(id) {
  ns <- NS(id)
  tagList(shinyjs::disabled(downloadButton(ns("dl_data"), label = "Data", title = "Download Raw Data File")))
}

collect_network_btns_ui <- function(id) {
  ns <- NS(id)
  tagList(shinyjs::disabled(downloadButton(ns("dl_network"), label = "Network", title = "Download Network Data File")))
}

collect_graph_btns_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::disabled(downloadButton(ns("dl_graph"), label = "Graphml", title = "Download Network Graphml File"))
  )
}

collect_view_graph_btns_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::disabled(actionButton(ns("view_graph"), label = "Graph", title = "View Network Graph", icon("eye")))
  )
}

collect_data_name_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::disabled(actionButton(ns("data_name_toggle"), label = "Graph", title = "View Network Graph", icon("eye")))
  )
}

collect_data_btns <- function(input, output, session, data, file_prefix = "") {
  rv <- reactive(data())
  
  output$dl_data <- downloadHandler(
    filename = function() {
      create_dtm_filename(paste0(ifelse(file_prefix == "", "", paste0(file_prefix, "-")), "data"), "rds")
    },
    content = function(file) {
      saveRDS(rv(), file)
    }
  )
  
  observeEvent(rv(), shinyjs::toggleState("dl_data", condition = isTruthy(rv())))
}

collect_network_btns <- function(input, output, session, network, file_prefix = "") {
  rv <- reactive(network())
  
  output$dl_network <- downloadHandler(
    filename = function() {
      create_dtm_filename(paste0(ifelse(file_prefix == "", "", paste0(file_prefix, "-")), "network"), "rds")
    },
    content = function(file) {
      saveRDS(rv(), file)
    }
  )
  observeEvent(rv(), shinyjs::toggleState("dl_network", condition = isTruthy(rv()))) 
}

collect_graph_btns <- function(input, output, session, graph, file_prefix = "") {
  rv <- reactive(graph())
  
  output$dl_graph <- downloadHandler(
    filename = function() {
      create_dtm_filename(ifelse(file_prefix == "", "graph", file_prefix), "graphml")
    },
    content = function(file) {
      write_graph(rv(), file, format = c("graphml"))
    }
  )
  
  observeEvent(rv(), {
    is_data <- isTruthy(rv())
    shinyjs::toggleState("dl_graph", condition = is_data)
    shinyjs::toggleState("view_graph", condition = is_data)
  })
}

collect_view_graph_btns <- function(input, output, session, graph) {
  rv <- reactiveValues(data = NULL)
  observeEvent(input$view_graph, rv$data <<- graph())
  return(rv)
}
