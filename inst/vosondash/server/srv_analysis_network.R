graph_rv <- reactiveValues(
  data = NULL,       # vosonsml df
  graph_data = NULL, # igraph graph object
  graph_seed = NULL, # plot seed value
  
  graph_desc = "",   # some graph attributes
  graph_name = "",
  graph_type = "",
  
  graph_dir = TRUE, # directed
  
  graph_cats = c(),        # list of categories in the data # graph_CA
  graph_cat_selected = "", # selected category # graph_CA_selected
  
  plot_height = gbl_plot_height,
  legend_height = 42,
  
  prune_nodes = c()
)

dt_prev_sel <- reactiveValues(nodes = c())

# proxy for nodes data table used for row manipulation
dt_nodes_proxy <- dataTableProxy("dt_nodes")

# disable network metrics and assortativity tabs when app loads
addCssClass(selector = "a[data-value = 'network_metrics_tab']", class = "inactive_menu_link")
addCssClass(selector = "a[data-value = 'assortativity_tab']", class = "inactive_menu_link")

source("server/srv_controls.R", local = TRUE)

#### events ---------------------------------------------------------------------------------------------------------- #

# set reactive value plot height when height input changes
observeEvent(input$plot_height, {
  graph_rv$plot_height <- input$plot_height
}, ignoreInit = TRUE)

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
    # cat(paste("error loading demo files:", err))
  }, warning = function(w) {
    # cat(paste("warning loading demo files:", w))
  })
}, once = TRUE)

# load demo data button event
observeEvent(input$demo_data_select_button, {
  load_file <- system.file("extdata", paste0(input$demo_data_select, ".graphml"), package = "VOSONDash")

  if (load_file != "") {
    file_desc <- "Description not found."
    tryCatch({
      file_desc <- paste(readLines(paste0(load_file, ".txt")), collapse = "<br>")
    }, error = function(err) {
      # cat(paste("error loading demo files:", err))
    }, warning = function(w) {
      # cat(paste("warning loading demo files:", w))
    })
    
    tryCatch({
      data <- igraph::read_graph(load_file, format = c('graphml'))
      type <- ifelse("type" %in% igraph::graph_attr_names(data), igraph::graph_attr(data, "type"), "")
      
      ng_set_view(data = data,
                     desc = file_desc,
                     type = type,
                     name = input$demo_data_select,
                     seed = get_random_seed())
    }, error = function(err) {
      # cat(paste("error loading demo files:", err))
    }, warning = function(w) {
      # cat(paste("warning loading demo files:", w))
    })
  }
})

# when graphml data loaded or changed
observeEvent(graph_rv$graph_data, {
  if (!is.null(graph_rv$graph_data)) {
    
    # add node ids and labels if not present
    attr_v <- igraph::vertex_attr_names(graph_rv$graph_data)
    if (!("id" %in% attr_v)) {
      igraph::V(graph_rv$graph_data)$id <- paste0("n", as.numeric(igraph::V(graph_rv$graph_data))-1) # n0, n1 ..
    }
    
    if ("label" %in% attr_v) {
      # replace empty string labels
      igraph::V(graph_rv$graph_data)$label <- ifelse(nchar(igraph::V(graph_rv$graph_data)$label) > 0, 
                                                  igraph::V(graph_rv$graph_data)$label, "-")
    } else {
      # if no labels set label to node name
      igraph::V(graph_rv$graph_data)$label <- ifelse(nchar(igraph::V(graph_rv$graph_data)$name) > 0,
                                                  igraph::V(graph_rv$graph_data)$name, "-")
    }
    
    # set directed
    isolate({ graph_rv$graph_dir <- igraph::is_directed(graph_rv$graph_data) })
    
    # enable network metrics tab
    removeCssClass(selector = "a[data-value = 'network_metrics_tab']", class = "inactive_menu_link")
    
    shinyjs::show("graph_download_button")
    shinyjs::show("analysis_graphml_download_button")
  }
})

# enable assortativity tab when a category other than "all" selected
observeEvent(graph_rv$graph_cat_selected, {
  if (graph_rv$graph_cat_selected %in% c("", "All")) {
    addCssClass(selector = "a[data-value = 'assortativity_tab']", class = "inactive_menu_link")
  } else {
    removeCssClass(selector = "a[data-value = 'assortativity_tab']", class = "inactive_menu_link")
  }
})

# ** check this is not redundant
# update component slider when graph component or category changed
observeEvent({ input$graph_component_type_select
               input$graph_sub_cats_select
               graph_rv$prune_nodes
               input$reset_on_change_check }, {
  
  g <- graph_rv$graph_data
  g <- applyPruneFilterSrv(g, graph_rv$prune_nodes)
  
  if (input$reset_on_change_check == TRUE) {
    g <- applyCategoricalFilters(g, input$graph_cat_select, input$graph_sub_cats_select)
  }

  updateComponentSlider(g, input$graph_component_type_select)
}, ignoreInit = TRUE)
  
# selected category updates select box with its attribute values
observeEvent(input$graph_cat_select, {
  graph_rv$graph_cat_selected <- input$graph_cat_select
  
  if (!is.null(graph_rv$graph_data)) {
    attr_choices <- c("All")
    
    if (input$graph_cat_select != "All") {
      attr_choices <- append(attr_choices, graph_rv$graph_cats[[input$graph_cat_select]])
    }
    
    # update list of values in select box
    updateSelectInput(session, "graph_sub_cats_select", choices = attr_choices, selected = "All")
    
    # enable select box control
    shinyjs::enable("graph_sub_cats_select")
  }
})

# set graph controls on graph tab changes
observeEvent(input$selected_graph_tab, {
  setGraphTabControls()
})

# graphml file uploaded
observeEvent(input$graphml_data_file, {
  setGraphFile()
  
  # set a random number to seed plots
  graph_rv$graph_seed <- get_random_seed()
  
  # reset controls and filters
  setGraphTabControls()
  setGraphFilterControls()
})

observeEvent(graph_rv$graph_seed, {
  updateNumericInput(session, "graph_seed_input", value = graph_rv$graph_seed)
})

# generate a new random seed on reseed button event
observeEvent(input$graph_reseed_button, graph_rv$graph_seed <- get_random_seed())

observeEvent(input$graph_seed_set_button, {
  if (is.numeric(input$graph_seed_input) && (input$graph_seed_input >= 1) && !is.infinite(input$graph_seed_input)) {
    graph_rv$graph_seed <- input$graph_seed_input
  }
})

observeEvent(input$node_index_check, {
  if (input$node_index_check) {
    updateCheckboxInput(session, "node_labels_check", value = FALSE)
  }
})

observeEvent(input$node_labels_check, {
  if (input$node_labels_check) {
    updateCheckboxInput(session, "node_index_check", value = FALSE)
  }  
})

observeEvent(input$node_label_select, {
  if (!is.null(graph_rv$graph_data)) {
    igraph::V(graph_rv$graph_data)$label <- igraph::vertex_attr(graph_rv$graph_data, input$node_label_select)
  }
})

#### graph node pruning ####

# add selected data table rows to pruned nodes list
observeEvent(input$prune_selected_rows_button, {
  # this updates prune list and triggers graph redraw
  pruneListAddNames()
  
  # update prune list select box
  prune_list <- isolate(graph_rv$prune_nodes)
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      # n_value <- V(isolate(graph_rv$graph_data))[which(V(isolate(graph_rv$graph_data))$id == i)]$label # name
      if (is.null(igraph::vertex_attr(isolate(graph_rv$graph_data), "label"))) {
        n_value <- V(isolate(graph_rv$graph_data))[which(V(isolate(graph_rv$graph_data))$id == i)]$name
      } else {
        n_value <- V(isolate(graph_rv$graph_data))[which(V(isolate(graph_rv$graph_data))$id == i)]$label
      }
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_nodes_select", choices = prune_list)
})

# add unselected data table rows to pruned nodes list
observeEvent({ input$prune_unselected_rows_button }, {
  pruneListAddOtherNames()
  
  # update prune list select box
  prune_list <- isolate(graph_rv$prune_nodes)
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      if (is.null(igraph::vertex_attr(isolate(graph_rv$graph_data), "label"))) {
        n_value <- igraph::V(isolate(graph_rv$graph_data))[which(igraph::V(isolate(graph_rv$graph_data))$id == i)]$name
      } else {
        n_value <- igraph::V(isolate(graph_rv$graph_data))[which(igraph::V(isolate(graph_rv$graph_data))$id == i)]$label
      }
      
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_nodes_select", choices = prune_list)
  # DT::selectRows(dt_nodes_proxy, rownames(graphNodes()))
})

observeEvent({ input$nbh_prune_unselected }, {
    pruneListAddOtherNames()
    
    prune_list <- isolate(graph_rv$prune_nodes)
    if (is.null(prune_list)) { 
      prune_list <- character(0) 
    } else {
      temp <- list()
      for (i in prune_list) {
        if (is.null(igraph::vertex_attr(isolate(graph_rv$graph_data), "label"))) {
          n_value <- V(isolate(graph_rv$graph_data))[which(V(isolate(graph_rv$graph_data))$id == i)]$name
        } else {
          n_value <- V(isolate(graph_rv$graph_data))[which(V(isolate(graph_rv$graph_data))$id == i)]$label
        }
        
        temp[paste0(i, " - ", n_value)] <- i
      }
      prune_list <- temp
    }
    updateSelectInput(session, "pruned_nodes_select", choices = prune_list)
  })

# remove selected nodes from prune list
observeEvent(input$prune_return_button, {
  graph_rv$prune_nodes <- graph_rv$prune_nodes[!(graph_rv$prune_nodes %in% input$pruned_nodes_select)]
  
  # update prune list select box
  prune_list <- isolate(graph_rv$prune_nodes)
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      if (is.null(igraph::vertex_attr(isolate(graph_rv$graph_data), "label"))) {
        n_value <- igraph::V(isolate(graph_rv$graph_data))[which(igraph::V(isolate(graph_rv$graph_data))$id == i)]$name
      } else {
        n_value <- igraph::V(isolate(graph_rv$graph_data))[which(igraph::V(isolate(graph_rv$graph_data))$id == i)]$label
      }
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_nodes_select", choices = prune_list)
})

# reset prune list
observeEvent({ input$prune_reset_button }, {
  graph_rv$prune_nodes <- c()
  updateSelectInput(session, "pruned_nodes_select", choices = character(0))
  
  # added to address bug with disappearing plot on pruning
  updateComponentSlider(graph_rv$graph_data, input$graph_component_type_select)  
})

observeEvent({ input$nbh_reset_button }, {
    graph_rv$prune_nodes <- c()
    updateSelectInput(session, "pruned_nodes_select", choices = character(0))
    updateComponentSlider(graph_rv$graph_data, input$graph_component_type_select)  
  })

# deselect all data table selected rows
observeEvent(input$prune_deselect_rows_button, { DT::selectRows(dt_nodes_proxy, NULL) })
observeEvent(input$nbh_deselct_button, { DT::selectRows(dt_nodes_proxy, NULL) })

# nodes clicked event in visnetwork
observeEvent(input$vis_node_select, {
  dt_nodes <- isolate(graphNodes())
  
  selected_rows <- row.names(dt_nodes)[c(input$dt_nodes_rows_selected)] # selected in dt
  plot_sel_nodes <- row.names(dt_nodes)[dt_nodes$name %in% input$vis_node_select] # selected in plot
  
  deselect_nodes <- plot_sel_nodes[plot_sel_nodes %in% selected_rows] # deselect if already selected in dt
  all_selected <- union(selected_rows, plot_sel_nodes)
  
  sel <- all_selected[!all_selected %in% deselect_nodes]
  sel <- which(rownames(dt_nodes) %in% sel) # require indices not row names
  
  DT::selectRows(dt_nodes_proxy, sel)
})

observeEvent(input$nbh_select_button, {
  if (length(input$dt_nodes_rows_selected) < 1) { return() }
  g <- graphFilters()
  dt_nodes <- graphNodes()
  sel_rows <- row.names(dt_nodes)[c(input$dt_nodes_rows_selected)]
  
  dt_prev_sel$nodes <- sel_rows
  shinyjs::enable("nbh_undo_button")
  
  sel_row_names <- V(g)[V(g)$id %in% sel_rows]$name
  
  order <- input$nbh_order_select
  g_ego <- make_ego_graph(g, order = order, nodes = sel_row_names, mode = "all", mindist = 0)
  ids <- unlist(sapply(g_ego, function(x) V(x)$id))
  sel <- which(rownames(dt_nodes) %in% ids)
  
  DT::selectRows(dt_nodes_proxy, sel)
})

observeEvent(input$nbh_undo_button, {
  if (length(dt_prev_sel$nodes) > 0) {
    DT::selectRows(dt_nodes_proxy, NULL)
    sel <- which(rownames(graphNodes()) %in% dt_prev_sel$nodes)
    DT::selectRows(dt_nodes_proxy, sel)
    
    dt_prev_sel$nodes <- c()
    shinyjs::disable("nbh_undo_button")
  }
})

# reset node size slider when changed to none
observeEvent(input$graph_node_size_select, {
  if (input$graph_node_size_select == "None") { 
    shinyjs::reset("graph_node_size_slider")
  }
})

# on change layout event
observeEvent(input$graph_layout_select, {
  shinyjs::reset("graph_spread_slider") # reset graph spread when a new layout is selected
  
  if (input$graph_layout_select == "Graphopt") { 
    shinyjs::reset("graph_charge")
    shinyjs::reset("graph_mass")
    shinyjs::reset("graph_spr_len")
    shinyjs::reset("graph_spr_const")
  }
})

#### output ---------------------------------------------------------------------------------------------------------- #

output$graph_name <- renderText({
  output <- ifelse(nchar(graph_rv$graph_name), graph_rv$graph_name, "not set")
  output <- paste("Name: ", output)
  
  if (nchar(graph_rv$graph_type)) {
    output <- paste0(output, "\nType: ", graph_rv$graph_type)
  } else {
    output <- paste0(output, "\nType: unknown")
  }
  
  return(output)
})

output$graph_desc <- renderText({
  if (nchar(graph_rv$graph_desc)) {
    return(HTML(graph_rv$graph_desc))
  }
  
  return(HTML("No description."))
})

output$plot_height_ui <- renderUI({
  tagList(div(div(
    div(selectInput("plot_height", label = NULL, 
                    choices = c("300px" = 300, "400px" = 400, "500px" = 500, "600px" = 600, "700px" = 700, 
                                "800px" = 800, "900px" = 900, "1000px" = 1000), 
                    multiple = FALSE, selectize = FALSE, selected = graph_rv$plot_height), 
        style = "width:100%;", align = "right"),
    style = "position:absolute; z-index:1; top:50px; right:30px; font-size:0.97em;"),
    style = "position:relative; z-index:0;"))
})

output$graph_summary_ui <- renderUI({
  tagList(div(div(
    HTML(graphSummaryOutput()),
    style = paste0("position:absolute; z-index:1; top:", (as.numeric(graph_rv$plot_height)-5), 
                   "px; left:18px; font-size:0.97em;")),
    style = "position:relative; z-index:0;"))
})

output$graph_legend_ui <- renderUI({
  tagList(div(div(
    HTML(graphLegendOutput()),
    style = paste0("position:absolute; z-index:1; top:", as.numeric(graph_rv$legend_height),
                   "px; left:18px; font-size:0.97em;")),
    style = "position:relative; z-index:0;"))
})

output$vis_plot_ui <- renderUI({
  tabBox(width = 12, title = span(icon("share-nodes", class = "social_green"), "Network Graphs"), 
         selected = input$selected_graph_tab, id = "selected_graph_tab",
         tabPanel("igraph", plotOutput("igraphPlot", width = "100%", height = "auto"), value = "Plot"),
         tabPanel("visNetwork", visNetworkOutput("visNetworkPlot", width = "100%",
                                                 height = paste0(graph_rv$plot_height, "px")), value = "visNetwork"),
         tabPanel(icon("circle-info"),
                  value = "graph_info",
                  htmlOutput("graph_info_text", style = paste0("height:", graph_rv$plot_height, "px")))
  )
})

output$component_summary_ui <- renderText({
  graphComponentSummary()
})

output$graph_info_text <- renderText({
  gname <- ifelse(nchar(graph_rv$graph_name), graph_rv$graph_name, "not set")
  output <- paste0("<p>Name: ", gname, "<br>")
  
  gtype <- ifelse(nchar(graph_rv$graph_type), graph_rv$graph_type, "unknown")
  output <- paste0(output, "Type: ", gtype, "<br>")
  
  gdesc <- ifelse(nchar(graph_rv$graph_desc), HTML(graph_rv$graph_desc), "No description.")
  output <- paste0(output, "Description:<br></p><p>", gdesc, "</p>")

  output
})

# graph download buttons
output$graph_download_button <- downloadHandler(
  filename = function() { saveGraphFileName() },
  
  content = function(file) {
    if (input$selected_graph_tab == "visNetwork") {
      visSave(saveGraphFileData(), file, selfcontained = TRUE, background = "white")
    } else {
      saveNetwork(saveGraphFileData(), file, selfcontained = TRUE) 
    }
  }
)

# analysis graphml download button
output$analysis_graphml_download_button <- downloadHandler(
  filename = function() { systemTimeFilename("analysis-graph", "graphml") },
  content = function(file) { write_graph(graphFilters(), file, format = c("graphml")) }
)

# graph nodes data table
output$dt_nodes <- DT::renderDataTable({
  data <- graphNodes()
  
  # truncate text in column cells
  col_defs <- NULL
  if (input$graph_dt_v_truncate_text_check == TRUE) {
    col_defs <- gbl_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  
  if (!is.null(data)) {
    dt <- DT::datatable(data, extensions = 'Buttons', filter = "top",
                        options = list(lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                        columnDefs = col_defs, dom = 'lBfrtip', buttons = c('copy', 'csv', 'excel', 'print')),
                        class = 'cell-border stripe compact hover')

    # format betweeness and closeness values to display 3 decimal places
    if (nrow(data) > 0) {
      dt <- DT::formatRound(dt, columns = c('betweenness', 'closeness'), digits = 3)
    }
    
    return(dt)
  }
  
  NULL
})

# graph edges data table
output$dt_edges <- DT::renderDataTable({
  data <- graphEdges()
  
  # truncate text in column cells
  col_defs <- NULL
  if (input$graph_dt_e_truncate_text_check == TRUE) {
    col_defs <- gbl_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  
  if (!is.null(data)) {
    DT::datatable(data, extensions = 'Buttons', filter = "top", selection = "none", # rows not selectable
                  options = list(lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                  columnDefs = col_defs, dom = 'lBfrtip',
                  buttons = c('copy', 'csv', 'excel', 'print')), class = 'cell-border stripe compact hover')
  }
})

output$igraphPlot <- renderPlot({
  igraphData()
}, height = function() { as.numeric(graph_rv$plot_height) })

output$visNetworkPlot <- renderVisNetwork({
  visNetworkData()
})

#### reactives -------------------------------------------------------------------------------------------------------- #

source("server/srv_plot_igraph.R", local = TRUE)
source("server/srv_plot_visnetwork.R", local = TRUE)

# set file data when a file is uploaded
setGraphFile <- reactive({
  infile <- input$graphml_data_file
  
  if (is.null(infile)) { return(NULL) }
  
  # reads file as graphml and fails gracefully
  tryCatch({
    graph_rv$graph_data <- igraph::read_graph(infile$datapath, format = c('graphml'))
    graph_rv$graph_name <- infile$name
    graph_rv$graph_type <- ifelse("type" %in% graph_attr_names(isolate(graph_rv$graph_data)), 
                                     graph_attr(isolate(graph_rv$graph_data), "type"), "")
    graph_rv$graph_desc <- "Network loaded from file."
    
    isolate({
      attr_v <- igraph::vertex_attr_names(graph_rv$graph_data)
      setLabels(attr_v)
      
      addNodeContinuous()
    })
    
    createGraphCategoryList()
    
    updateCheckboxInput(session, "expand_demo_data_check", value = FALSE)
  }, error = function(err) { return(NULL) })
})

# apply all filters to graph data and return modified graph
graphFilters <- reactive({
  g <- NULL

  if (!is.null(graph_rv$graph_data)) {
    g <- graph_rv$graph_data
    
    # ----
    # add node ids and labels if not present
    attr_v <- igraph::vertex_attr_names(g)
    if (!("id" %in% attr_v)) {
      igraph::V(g)$id <- paste0("n", as.numeric(igraph::V(g))-1) # n0, n1 ..
    }
    
    if ("label" %in% attr_v) {
      # replace empty string labels
      igraph::V(g)$label <- ifelse(nchar(igraph::V(g)$label) > 0, igraph::V(g)$label, "-")
    } else {
      # if no labels set label to node name
      igraph::V(g)$label <- ifelse(nchar(igraph::V(g)$name) > 0, igraph::V(g)$name, "-")
    }    
    # ----
    
    g <- applyPruneFilterSrv(g, graph_rv$prune_nodes)
    g <- applyCategoricalFilters(g, input$graph_cat_select, input$graph_sub_cats_select)
    # isolate as graph_component_type_select has event
    g <- applyComponentFilter(g, isolate(input$graph_component_type_select), input$graph_component_slider)    
    g <- applyGraphFilters(g, input$graph_isolates_check, input$graph_multi_edge_check, 
                                      input$graph_loops_edge_check)
    g <- addAdditionalMeasures(g)
    
    # enable network metrics tab
    removeCssClass(selector = "a[data-value = 'network_metrics_tab']", class = "inactive_menu_link")
  }
  
  return(g)
})

# create a list of categories from voson node category field names in data
createGraphCategoryList <- reactive({
  graph_rv$graph_cats <- getNodeCategories(graph_rv$graph_data)  
})

# only runs on file upload or when collection view graph option selected
setGraphFilterControls <- reactive({
  g <- graph_rv$graph_data
  
  # reset pruned list
  graph_rv$prune_nodes <- c()
  updateSelectInput(session, "pruned_nodes_select", choices = character(0))
  
  # clear text analysis plot list
  ta_rv$plot_data_list <- NULL
  
  if (is.null(g)) {
    # disable controls if no data
    disableGraphFilterControls()
    disableTextAnalysisControls()
    
    shinyjs::hide("graph_download_button")
    shinyjs::hide("analysis_graphml_download_button")
    
    return(NULL)
  }
  
  if (vcount(g) > 0) {
    # reset and enable graph filter controls
    resetEnableGraphFilterControls()
    
    shinyjs::show("analysis_graphml_download_button")
    
    shinyjs::enable("graph_reseed_button")
    shinyjs::enable("graph_component_slider")

    updateComponentSlider(g, isolate(input$graph_component_type_select))
    
    dt_prev_sel$nodes <- c()
    shinyjs::reset("nbh_undo_button")
    
    # update the categorical attribute select box
    if (!is.null(graph_rv$graph_cats) && length(graph_rv$graph_cats) > 0) {
      shinyjs::reset("graph_cat_select")
      shinyjs::enable("graph_cat_select")
      
      category_choices <- c("All")
      category_choices <- append(category_choices, names(graph_rv$graph_cats))
      
      updateSelectInput(session, "graph_cat_select", choices = category_choices, selected = "All")
      
      shinyjs::reset("reset_on_change_check")
      shinyjs::enable("reset_on_change_check")
      
    } else {
      shinyjs::reset("graph_cat_select")
      shinyjs::disable("graph_cat_select")
      
      shinyjs::reset("graph_sub_cats_select")
      shinyjs::disable("graph_sub_cats_select")   
      
      shinyjs::reset("reset_on_change_check")
      shinyjs::disable("reset_on_change_check")      
    }
    
    # text analysis controls
    if (hasVosonTextData(g)) {
      ta_rv$has_text <- TRUE
      
      # reset and enable text analysis controls
      resetEnableTextAnalysisControls()
      
    } else {
      ta_rv$has_text <- FALSE
      
      # disable controls if no text data
      disableTextAnalysisControls()
    }
  }
})

# graph tab specific controls
setGraphTabControls <- reactive({
  g <- graph_rv$graph_data
  visnet_control_offset <- ifelse(input$visnet_id_select_check, 85, 42)
  
  # disable controls
  if (is.null(g)) {
    shinyjs::hide("graph_download_button")
    shinyjs::disable("graph_reseed_button")
    
    return(NULL)
  }
  
  # enable or disable controls based on network graph tab
  switch(input$selected_graph_tab,
         "Plot" = {
           graph_rv$legend_height <- 42
           enablePlotControls()
           shinyjs::hide("graph_download_button")
           shinyjs::show("analysis_graphml_download_button")
           shinyjs::show("plot_height")
           shinyjs::show("graph_legend_ui")
           shinyjs::show("graph_summary_ui")
           shinyjs::disable("visnet_id_select_check")
         },
         "visNetwork" = {
           graph_rv$legend_height <- visnet_control_offset
           enableVisNetworkControls()
           shinyjs::show("graph_download_button")
           shinyjs::show("analysis_graphml_download_button")
           shinyjs::show("plot_height")
           shinyjs::show("graph_legend_ui")
           shinyjs::show("graph_summary_ui")
           shinyjs::enable("visnet_id_select_check")
         },
         "graph_info" = {
           # hide plot canvas features
           shinyjs::hide("graph_download_button")
           shinyjs::hide("analysis_graphml_download_button")
           shinyjs::hide("plot_height")
           shinyjs::hide("graph_legend_ui")
           shinyjs::hide("graph_summary_ui")
           shinyjs::disable("visnet_id_select_check")
         }
        )
})

# network graph save file name based on selected network graph tab
saveGraphFileName <- reactive({
  switch(input$selected_graph_tab,
         "visNetwork" = systemTimeFilename("visNetwork-graph", "html"))
})

# network graph data based on selected network graph tab
saveGraphFileData <- reactive({
  data <- switch(input$selected_graph_tab,
         "visNetwork" = visNetworkData())
  
  if (input$selected_graph_tab == "visNetwork") {
    data$height <- "800px"
    data$sizingPolicy$defaultWidth <- "100%"
    
    data$sizingPolicy$browser$fill <- TRUE
    data$sizingPolicy$viewer$suppress <- TRUE
    data$sizingPolicy$knitr$figure <- FALSE    
  }
  
  data
})

# add selected data table row name values to pruned nodes list
pruneListAddNames <- reactive({
  dt_nodes <- isolate(graphNodes())
  dt_selected_rows <- input$dt_nodes_rows_selected
  prune_list <- graph_rv$prune_nodes
  
  selected_rows <- row.names(dt_nodes)[c(dt_selected_rows)]
  
  # add name if not already in list
  lapply(selected_rows, function(x) {
                          if (!x %in% graph_rv$prune_nodes)
                            graph_rv$prune_nodes <<- append(graph_rv$prune_nodes, x)})
})

# add deselected data table row name values to pruned nodes list
pruneListAddOtherNames <- reactive({
  dt_nodes <- isolate(graphNodes())
  dt_selected_rows <- input$dt_nodes_rows_selected
  prune_list <- graph_rv$prune_nodes
  
  # does not let user prune all data this way requires two or more selected rows
  if (length(dt_selected_rows) > 1) {
    selected_rows <- row.names(dt_nodes)[c(dt_selected_rows)]
    
    # names of nodes not selected
    sdf <- subset(dt_nodes, !(row.names(dt_nodes) %in% selected_rows))
    selected_rows <- row.names(sdf)
    
    # add name if not already in list
    lapply(selected_rows, function(x) {
                            if (!x %in% graph_rv$prune_nodes) 
                              graph_rv$prune_nodes <<- append(graph_rv$prune_nodes, x)}) 
  }
})

# graph nodes as dataframe
graphNodes <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) { return(NULL) }

  df_parameters <- list()
  
  df_parameters[["name"]] <- igraph::V(g)$name
  if (!(is.null(igraph::vertex_attr(g, "label")))) { df_parameters[["label"]] <- igraph::V(g)$label }
  if ("color" %in% igraph::vertex_attr_names(g)) { df_parameters[["color"]] <- igraph::V(g)$color }
  
  if (input$mast_images) {
    if ("user.avatar" %in% igraph::vertex_attr_names(g)) {
      df_parameters[["user.avatar"]] <- igraph::V(g)$user.avatar
    }
  }
  
  df_parameters[["degree"]] <- igraph::V(g)$Degree
  df_parameters[["indegree"]] <- igraph::V(g)$Indegree
  df_parameters[["outdegree"]] <- igraph::V(g)$Outdegree
  df_parameters[["betweenness"]] <- igraph::V(g)$Betweenness
  df_parameters[["closeness"]] <- igraph::V(g)$Closeness
  
  attr_v <- igraph::vertex_attr_names(g)
  voson_txt_attrs <- attr_v[grep(voson_txt_prefix, attr_v, perl = T)]
  if (length(voson_txt_attrs)) {
    attr <- voson_txt_attrs[1]
    df_txt_attr <- gsub(voson_txt_prefix, "", attr, perl = TRUE)
    df_parameters[[df_txt_attr]] <- igraph::vertex_attr(g, attr, index = V(g))
  }
  
  voson_cat_attrs <- attr_v[grep(voson_cat_prefix, attr_v, perl = T)]
  if (length(voson_cat_attrs) > 0) {
    for (i in 1:length(voson_cat_attrs)) {
      attr <- voson_cat_attrs[i]
      df_txt_attr <- gsub(voson_cat_prefix, "", attr, perl = TRUE) # vosonCA_
      df_parameters[[df_txt_attr]] <- igraph::vertex_attr(g, attr, index = V(g))
    }  
  }

  for (attr in attr_v) {
    values <- igraph::vertex_attr(g, attr)
    if (is.numeric(values) &
        (!attr %in% voson_txt_attrs) &
        (!attr %in% voson_cat_attrs) &
        (!attr %in% names(df_parameters)) &
        (!tolower(attr) %in% names(df_parameters))) {
      df_parameters[[attr]] <- values
    }
  }
  
  df_parameters["stringsAsFactors"] <- FALSE
  df <- do.call(data.frame, df_parameters)
  
  row.names(df) <- igraph::V(g)$id
  
  return(df)
})

# graph edges as dataframe
graphEdges <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) { return(NULL) }
  
  igraph::as_data_frame(g, what = c("edges"))
})

# graph summary
graphSummaryOutput <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    output <- append(output, paste0("Nodes: ", vcount(g)))
    output <- append(output, paste0("Edges: ", ecount(g)))
    
    isolate_count <- sum(degree(g) == 0)
    output <- append(output, paste0("Isolates: ", isolate_count))
  }else {
    output <- append(output, paste0(""))
  }
  
  paste0(output, collapse = '<br>')
})

dlGraphButtonOutput <- reactive({
  tagList(tags$div(downloadButton("analysis_graphml_download_button", label = "Graphml", title = "Download Plot Graphml File"), 
      style = "float:right; margin-right:10px; margin-bottom:0px;", class = "div_inline"),
      tags$div(downloadButton("graph_download_button", label = "Plot HTML", title = "Download Plot as HTML File"), 
      style = "float:right; margin-right:10px; margin-bottom:0px;", class = "div_inline"))
})

output$graph_dl_button_ui <- renderUI({
  tagList(div(div(
    dlGraphButtonOutput(),
    style = paste0("position:absolute; z-index:1; top:", (as.numeric(graph_rv$plot_height)+16),
                   "px; right:14px; font-size:0.9em;")),
    style = "position:relative; z-index:0;"))
})

graphLegendOutput <- reactive({
  if (input$graph_legend_check == FALSE) { return("") }
  g <- graphFilters()
  output <- c()
  
  if (!is.null(g)) {
    isolate({
      categorical_attributes <- graph_rv$graph_cats
      selected_categorical_attribute <- input$graph_cat_select
    })
    
    output <- append(output, paste0(""))
    
    if (length(categorical_attributes) > 0) {
      if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
        categories <- categorical_attributes[[selected_categorical_attribute]]
        df <- data.frame('cat' = categories)
        if (nrow(df) > 0) {
          if (!("color" %in% igraph::vertex_attr_names(g) & input$use_node_colors_check == TRUE)) {
            output <- append(output, paste0("<table><tbody><tr><td colspan='3'>",
                                            selected_categorical_attribute, "</td></tr>"))
            df$color <- gbl_plot_palette()[1:nrow(df)]
            for (row in 1:nrow(df)) {
              output <- append(output,
                paste0("<tr><td style='vertical-align:middle'>",
                  "<span style='height:12px; width:12px; border-radius:50%; display:inline-block;",
                  "background-color:", df[row, 2], ";'></span></td>",
                  "<td>&nbsp;</td><td style='vertical-align:middle'>", df[row, 1], "</td></tr>"))
            }
            output <- append(output, "</tbody></table>")
          }
        }
      }
    }
    
  } else {
    output <- append(output, paste0(""))
  }
  
  output
})

graphComponentSummary <- reactive({
  g <- graphFilters()
  
  output <- c()
  
  if (!is.null(g)) {
    graph_clusters <- components(g, mode = isolate(input$graph_component_type_select))
    
    output <- append(output, paste0("Components (", isolate(input$graph_component_type_select), "): ", 
                                    graph_clusters$no))
    
    min_value <- max_value <- 0
    if (graph_clusters$no > 0) {
      # suppress no non-missing arguments to min; returning Inf warning
      min_value <- suppressWarnings(min(graph_clusters$csize))
      max_value <- suppressWarnings(max(graph_clusters$csize))
    }
    
    if (graph_clusters$no == 1) {
      output <- append(output, paste0("Size: ", min_value, sep = ""))
    } else {
      output <- append(output, paste0("Size min: ", min_value, " max: ", max_value, sep = ""))
    }
  }else {
    output <- append(output, paste0(""))
  }
  
  paste0(output, collapse = '\n')
})

#### functions ------------------------------------------------------------------------------------------------------- #

addNodeContinuous <- function() {
  if (!is.null(graph_rv$graph_data)) {
    
    add_attrs <- sapply(igraph::vertex_attr_names(graph_rv$graph_data),
                        function(x) if (all(sapply(igraph::vertex_attr(graph_rv$graph_data, x), is.numeric))) x)
    
    updateSelectInput(session, "graph_node_size_select", label = NULL,
                      choices = append(c("None", "Degree", "Indegree", "Outdegree", "Betweenness", "Closeness"), add_attrs),
                      selected = "None")
  }
}

setLabels <- function(attr_v) {
  sel <- NULL
  if ("label" %in% attr_v) {
    V(graph_rv$graph_data)$imported_Label <- V(graph_rv$graph_data)$label
    attr_v <- append(attr_v, "imported_Label")
    sel <- "imported_Label"
  }
  label_list <- sort(attr_v[!attr_v %in% c("label")])
  if (is.null(sel)) { sel <- "id" }
  shinyjs::enable("node_label_select")
  updateSelectInput(session, "node_label_select", label = NULL, choices = label_list, selected = sel)
}

# set graph manually
ng_set_view <- function(data, desc = "", type = "", name = "", seed = 1) {
  shinyjs::reset("graphml_data_file")
  
  graph_rv$graph_data <- data
  graph_rv$graph_desc <- desc
  graph_rv$graph_type <- type
  graph_rv$graph_name <- name
  graph_rv$graph_seed <- seed
  
  attr_v <- igraph::vertex_attr_names(data)
  setLabels(attr_v)
  
  addNodeContinuous()
  
  graph_rv$graph_cats <- c()
  graph_rv$graph_cat_selected <- ""
  
  createGraphCategoryList()
  setGraphFilterControls()
  
  updateNavbarPage(session, "nav_sel_tab_id", selected = "network_graphs_tab")
}

updateComponentSlider <- function(g, component_type) {
  if (!is.null(g)) {
    graph_clusters <- components(g, mode = component_type)
    
    # suppress no non-missing arguments to min; returning Inf warning
    min_cluster_size <- suppressWarnings(min(graph_clusters$csize))
    max_cluster_size <- suppressWarnings(max(graph_clusters$csize))
    
    # likely causes a double render when graph has a component max size greater than the initial slider max set in ui
    updateSliderInput(session, inputId = "graph_component_slider", min = min_cluster_size,
                      max = max_cluster_size, value = c(min_cluster_size, max_cluster_size))
  }
}

# filter out list of nodes from graph object
applyPruneFilterSrv <- function(g, selected_prune_nodes) {
  if (length(selected_prune_nodes) > 0) {
    nodes <- which(igraph::V(g)$id %in% selected_prune_nodes)
    g <- igraph::delete_vertices(g, nodes) # selected_prune_nodes
  }
  return(g)
}

# normalize continuous values
norm_values <- function(x) {
  # all values the same
  if (var(x) == 0) return(rep(0.1, length(x)))
  
  min_x <- min(x)
  diff_x <- max(x) - min_x
  s <- sapply(x, function(y) ((y - min_x) / diff_x))
}
