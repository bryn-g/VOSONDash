dt_prev_sel <- reactiveValues(nodes = c())

# add selected data table rows to pruned nodes list
observeEvent(input$prune_selected_rows_button, {
  g <- r_graph_base()
  
  # this updates prune list and triggers graph redraw
  pruneListAddNames()
  
  # update prune list select box
  prune_list <- graph_rv$prune_nodes
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      if (is.null(igraph::vertex_attr(g, "label"))) {
        n_value <- V(g)[which(V(g)$id == i)]$name
      } else {
        n_value <- V(g)[which(V(g)$id == i)]$label
      }
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_nodes_select", choices = prune_list)
})

# add unselected data table rows to pruned nodes list
observeEvent(input$prune_unselected_rows_button, {
  g <- r_graph_base()
  
  pruneListAddOtherNames()
  
  # update prune list select box
  prune_list <- graph_rv$prune_nodes
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      if (is.null(igraph::vertex_attr(g, "label"))) {
        n_value <- igraph::V(g)[which(igraph::V(g)$id == i)]$name
      } else {
        n_value <- igraph::V(g)[which(igraph::V(g)$id == i)]$label
      }
      
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_nodes_select", choices = prune_list)
})

observeEvent(input$nbh_prune_unselected, {
  g <- r_graph_base()
  
  pruneListAddOtherNames()
  
  prune_list <- graph_rv$prune_nodes
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      if (is.null(igraph::vertex_attr(g, "label"))) {
        n_value <- V(g)[which(V(g)$id == i)]$name
      } else {
        n_value <- V(g)[which(V(g)$id == i)]$label
      }
      
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_nodes_select", choices = prune_list)
})

# remove selected nodes from prune list
observeEvent(input$prune_return_button, {
  g <- r_graph_base()
  
  graph_rv$prune_nodes <- graph_rv$prune_nodes[!(graph_rv$prune_nodes %in% input$pruned_nodes_select)]
  
  # update prune list select box
  prune_list <- graph_rv$prune_nodes
  if (is.null(prune_list)) { 
    prune_list <- character(0) 
  } else {
    temp <- list()
    for (i in prune_list) {
      if (is.null(igraph::vertex_attr(g, "label"))) {
        n_value <- igraph::V(g)[which(igraph::V(g)$id == i)]$name
      } else {
        n_value <- igraph::V(g)[which(igraph::V(g)$id == i)]$label
      }
      temp[paste0(i, " - ", n_value)] <- i
    }
    prune_list <- temp
  }
  updateSelectInput(session, "pruned_nodes_select", choices = prune_list)
})

# reset prune list
observeEvent(input$prune_reset_button, {
  g <- req(r_graph_base())
  
  graph_rv$prune_nodes <- c()
  updateSelectInput(session, "pruned_nodes_select", choices = character(0))
  
  # added to address bug with disappearing plot on pruning
  # f_update_comp_slider_range(g, input$graph_comp_type_sel)
  f_set_comp_ranges(g)
  comp_rv$mode <- input$graph_comp_type_sel
  f_set_comp_slider_range()
})

observeEvent(input$nbh_reset_button, {
  graph_rv$prune_nodes <- c()
  updateSelectInput(session, "pruned_nodes_select", choices = character(0))
  #f_update_comp_slider_range(graph_rv$data, input$graph_comp_type_sel)  
})

# deselect all data table selected rows
observeEvent(input$prune_deselect_rows_button, { DT::selectRows(dt_nodes_proxy, NULL) })
observeEvent(input$nbh_deselct_button, { DT::selectRows(dt_nodes_proxy, NULL) })

# nodes clicked event in visnetwork
observeEvent(input$vis_node_select, {
  dt_nodes <- r_graph_nodes_df()
  
  selected_rows <- row.names(dt_nodes)[c(input$dt_nodes_rows_selected)] # selected in dt
  plot_sel_nodes <- row.names(dt_nodes)[dt_nodes$name %in% input$vis_node_select] # selected in plot
  
  deselect_nodes <- plot_sel_nodes[plot_sel_nodes %in% selected_rows] # deselect if already selected in dt
  all_selected <- union(selected_rows, plot_sel_nodes)
  
  sel <- all_selected[!all_selected %in% deselect_nodes]
  sel <- which(rownames(dt_nodes) %in% sel) # require indices not row names
  
  DT::selectRows(dt_nodes_proxy, sel)
})

observeEvent(input$nbh_select_button, {
  req(length(input$dt_nodes_rows_selected) > 0)
  
  g <- r_graph_filtered()

  dt_nodes <- r_graph_nodes_df()
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
    sel <- which(rownames(r_graph_nodes_df()) %in% dt_prev_sel$nodes)
    DT::selectRows(dt_nodes_proxy, sel)
    
    dt_prev_sel$nodes <- c()
    shinyjs::disable("nbh_undo_button")
  }
})

# add selected data table row name values to pruned nodes list
pruneListAddNames <- reactive({
  dt_nodes <- isolate(r_graph_nodes_df())
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
  dt_nodes <- isolate(r_graph_nodes_df())
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
      if (!x %in% prune_list) 
        graph_rv$prune_nodes <<- append(graph_rv$prune_nodes, x)}) 
  }
})

# # filter out list of nodes from graph object
# applyPruneFilterSrv <- function(g, selected_prune_nodes) {
#   if (length(selected_prune_nodes) > 0) {
#     nodes <- which(igraph::V(g)$id %in% selected_prune_nodes)
#     g <- igraph::delete_vertices(g, nodes) # selected_prune_nodes
#   }
#   return(g)
# }
