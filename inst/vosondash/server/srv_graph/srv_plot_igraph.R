r_graph_igraph_plot <- reactive({
  g <- tryCatch({ 
    r_graph_filter()
  }, error = function(err) {
    NULL
  })
  
  if (is.null(g)) return(VOSONDash::get_empty_plot("No graph data."))
  if (igraph::gorder(g) <= 0) return(VOSONDash::get_empty_plot("No nodes to plot."))
  
  # isolate dependencies of r_graph_filters
  isolate({
    node_cats <- g_nodes_rv$cats
    node_cat_selected <- g_nodes_rv$cat_selected
    
    label_attr <- g_nodes_rv$label_selected
    label_type <- g_nodes_rv$label_type
  })
  
  sel_node_rows <- input$dt_nodes_rows_selected
  g_spread <- input$igraph_spread_slider  
  node_size_attr <- input$node_size_sel
  node_size_mplr <- input$node_size_slider  
  
  # save and restore graphics parameters
  saved_par <- par(no.readonly = TRUE)  
  
  # avoid unknown font warnings on windows by setting a tt font
  saved_win_font <- NULL
  if (.Platform$OS.type == "windows") {
    saved_win_font <- windowsFonts()$Arial
    windowsFonts(Arial = windowsFont("TT Arial"))
  }
  
  # restore previously saved pars and font on exit
  on.exit({
    par(saved_par)
    if (!is.null(saved_win_font)) windowsFonts(Arial = windowsFont(saved_win_font))
  })
  
  # df representation of graph nodes
  graph_nodes_df <- data.frame(
    name = igraph::V(g)$name, 
    degree = igraph::V(g)$Degree, 
    indegree = igraph::V(g)$Indegree, 
    outdegree = igraph::V(g)$Outdegree, 
    betweenness = igraph::V(g)$Betweenness, 
    closeness = igraph::V(g)$Closeness,
    stringsAsFactors = FALSE
  )
  row.names(graph_nodes_df) <- igraph::V(g)$id
  
  if ("label" %in% igraph::vertex_attr_names(g)) graph_nodes_df$label <- igraph::V(g)$label
  
  # check if node color attribute present
  has_color_attr <- ifelse(("color" %in% igraph::vertex_attr_names(g)), TRUE, FALSE)
  
  # set node color
  if (input$node_use_g_cols_chk == FALSE) {
    igraph::V(g)$color <- input$node_color
  } else {
    if (!has_color_attr) {
      igraph::V(g)$color <- input$node_color
    }
  }
  
  # node colors if nodes have cat attrs and one is selected
  if (isTruthy(isolate(input$fltr_cat_chk))) {
    if (length(node_cats) > 0) {
      if (nchar(node_cat_selected) && node_cat_selected != "All") {
        
        node_cats <- node_cats[[node_cat_selected]]
        node_cats_df <- data.frame("cat" = node_cats)
        if (nrow(node_cats_df) > 0) {
          if (input$node_use_g_cols_chk == FALSE || !has_color_attr) {
            node_cats_df$color <- gbl_plot_palette()[1:nrow(node_cats_df)]
            cat_attr <- paste0("vosonCA_", node_cat_selected)
            igraph::V(g)$color <- node_cats_df$color[match(igraph::vertex_attr(g, cat_attr), node_cats_df$cat)]  
          }
        }
  
      }
    }
  }
  
  sel_node_row_names <- c()
  if (length(sel_node_rows) > 0) {
    sel_node_row_names <- row.names(graph_nodes_df)[c(sel_node_rows)]
  }
  
  # create a list for plot parameters
  igraph_params <- list(g)
  
  # edge attributes
  igraph_params["edge.arrow.size"] <- input$edge_arrow_size
  igraph_params["edge.arrow.width"] <- input$edge_arrow_width
  igraph_params["edge.width"] <- input$edge_width
  igraph_params["edge.color"] <- input$edge_color
  igraph_params["edge.curved"] <- input$edge_curved
  
  # set node color for nodes selected in nodes data table
  igraph_params[["vertex.color"]] <- ifelse(V(g)$id %in% sel_node_row_names, gbl_plot_sel_node_color, V(g)$color)
  igraph_params[["vertex.frame.color"]] <- ifelse(V(g)$id %in% sel_node_row_names, "#000000", "gray")
  igraph_params[["vertex.label.font"]] <- ifelse(V(g)$id %in% sel_node_row_names, 2, 1)
  
  # node size
  base_node_size <-input$igraph_node_base_size_slider
  
  # multiplier for normalized node sizes
  norm_mplr <- 3
  igraph_node_size <- function(x) base_node_size + (((f_norm_vals(x) + 0.1) * norm_mplr) * node_size_mplr)
  
  # set node size
  if (node_size_attr != "None") {
    node_size <- igraph_node_size(igraph::vertex_attr(g, node_size_attr))
  } else {
    node_size <- ((base_node_size + 0.1) * node_size_mplr)
  }
  igraph_params[["vertex.size"]] <- node_size
  
  # node labels
  
  # label.degree defines the position of the node labels, relative to the center of the nodes
  # it is interpreted as an angle in radian, zero means 'to the right', and 'pi' means to the left,
  # up is -pi/2 and down is pi/2. the default value is -pi/4.
  
  label_type <- isolate(g_nodes_rv$label_type)
  
  if (label_type %in% c("index", "attribute")) {
    
    # set font family
    igraph_params["vertex.label.family"] <- igraph_params["edge.label.family"] <- "Arial"
    
    # set unicode font for non windows - ADD TO UI
    not_win <- .Platform$OS.type != "windows"
    arial_unicode <- "Arial Unicode MS" %in% VOSONDash::get_sysfont_names()
    
    if (not_win & arial_unicode & input$macos_font_chk) {
      igraph_params["vertex.label.family"] <- igraph_params["edge.label.family"] <- "Arial Unicode MS"
    }
    
    base_label_size <- 0.8
    
    # attribute labels
    if (label_type == "attribute") {
      
      # if displaying labels for selected nodes only
      if (input$node_sel_labels_chk) {
        sel_nodes <- sel_node_row_names # input$dt_nodes_rows_selected
        labels <- ifelse(V(g)$id %in% sel_nodes, V(g)$label, NA)
        
        # label all nodes
      } else {
        labels <- V(g)$label
      }
      
      # set node labels
      igraph_params[["vertex.label"]] <- labels
      label_color <- input$node_label_color
      
      igraph_params[["vertex.label.color"]] <- ifelse(
        V(g)$id %in% sel_node_row_names, gbl_sel_label_col, 
        ifelse(is.null(label_color), gbl_plot_def_label_color, label_color)
      )
      
      label_dist <- input$node_label_dist
      label_degree <- input$node_label_rot
      
      base_label_size <- input$node_label_size
      
      igraph_params["vertex.label.cex"] <- base_label_size
      igraph_params["vertex.label.dist"] <- label_dist
      igraph_params["vertex.label.degree"] <- label_degree
      
      # if node label size proportional to node size
      if (input$node_label_prop_chk) {
        igraph_params[["vertex.label.cex"]] <- switch(
          node_size_attr,
          "Degree" = (f_norm_vals(V(g)$Degree)) + base_label_size,
          "Indegree" = (f_norm_vals(V(g)$Indegree)) + base_label_size,
          "Outdegree" = (f_norm_vals(V(g)$Outdegree)) + base_label_size,
          "Betweenness" = (f_norm_vals(V(g)$Betweenness)) + base_label_size,
          "Closeness" = (f_norm_vals(V(g)$Closeness)) + base_label_size,
          "None" = base_label_size
        )  
      } else {
        igraph_params[["vertex.label.cex"]] <- input$node_label_size  # node label size
      }
      
    } else {
      igraph_params[["vertex.label.color"]] <- "#000000"
      base_node_size <- 7
    }
  } else {
    igraph_params[["vertex.label"]] <- NA
  }
  
  # set edge labels
  igraph_params[["edge.label"]] <- NA
  if (input$edge_labels_chk == TRUE & !is.null(input$edge_label_sel)) {
    igraph_params["edge.label.cex"] <- input$edge_label_size
    igraph_params[["edge.label"]] <- edge_attr(g, input$edge_label_sel)
    igraph_params[["edge.label.color"]] <- input$edge_label_color
  }
  
  graph_layout <- as.matrix(g_layout_rv$coords)
    
  # graph spread option changes scale
  # graph_layout <- igraph::norm_coords(graph_layout, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  x <- input$igraph_x_slider
  y <- input$igraph_y_slider
  
  graph_layout <- igraph::norm_coords(graph_layout, ymin = y[1], ymax = y[2], xmin = x[1], xmax = x[2])
  igraph_params["rescale"] <- FALSE
  
  if (isTruthy(g_spread)) graph_layout <- graph_layout * g_spread
  
  igraph_params[["layout"]] <- graph_layout

  # plot graph
  par(mar = rep(0, 4))
  do.call(plot.igraph, igraph_params)
})
