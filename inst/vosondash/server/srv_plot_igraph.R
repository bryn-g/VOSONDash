# create graph data for a standard network plot
igraphData <- reactive({
  g <- graphFilters()
  
  if (is.null(g)) return(emptyPlotMessage("No graph data."))
  if (vcount(g) <= 0) return(emptyPlotMessage("No nodes to plot."))
  
  # reactive dependencies
  isolate({
    # already dependencies of graphFilters
    categorical_attributes <- graph_rv$graph_cats
    selected_categorical_attribute <- input$graph_cat_select
  })
  
  selected_rows <- input$dt_nodes_rows_selected
  selected_edge_rows <- input$dt_edges_rows_selected
  chosen_layout <- input$graph_layout_select
  graph_seed <- graph_rv$graph_seed
  graph_spread <- input$graph_spread_slider  
  node_degree_type <- input$graph_node_size_select
  node_size_multiplier <- input$graph_node_size_slider  
  
  node_index_check <- input$node_index_check
  
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
  
  # if label attribute null set to name
  if (is.null(igraph::V(g)$label)) igraph::V(g)$label <- igraph::V(g)$name
  
  # df representation of nodes
  df <- data.frame(
    label = igraph::V(g)$label,
    name = igraph::V(g)$name, 
    degree = igraph::V(g)$Degree, 
    indegree = igraph::V(g)$Indegree, 
    outdegree = igraph::V(g)$Outdegree, 
    betweenness = igraph::V(g)$Betweenness, 
    closeness = igraph::V(g)$Closeness,
    stringsAsFactors = FALSE
  )
  row.names(df) <- igraph::V(g)$id
  graph_nodes <- df
  
  # check if node color attribute present
  v_color_in_data <- ifelse(("color" %in% igraph::vertex_attr_names(g)), TRUE, FALSE)
  
  # set node color
  if (input$use_node_colors_check == FALSE) {
    igraph::V(g)$color <- as.character(gbl_plot_def_node_color)  
  } else {
    if (!v_color_in_data) {
      igraph::V(g)$color <- as.character(gbl_plot_def_node_color)
    }
  }
  
  # node colors if nodes have cat attributes and one is selected
  if (length(categorical_attributes) > 0) {
    if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
      
      categories <- categorical_attributes[[selected_categorical_attribute]]
      df <- data.frame('cat' = categories)
      if (nrow(df) > 0) {
        if (input$use_node_colors_check == FALSE || !v_color_in_data) {
          df$color <- gbl_plot_palette()[1:nrow(df)]
          va <- paste0('vosonCA_', selected_categorical_attribute)
          igraph::V(g)$color <- df$color[match(igraph::vertex_attr(g, va), df$cat)]  
        }
      }

    }
  }
  
  selected_row_names <- c()
  
  # get any selected rows in nodes data table
  if (!is.null(selected_rows)) {
    if (length(selected_rows) > 0) row.names(graph_nodes)[c(selected_rows)]
  }
  
  # create a list for plot parameters
  plot_parameters <- list(g)
  
  # edge attributes - ADD TO UI
  plot_parameters["edge.arrow.size"] <- input$edge_arrow_size
  plot_parameters["edge.arrow.width"] <- input$edge_arrow_width
  plot_parameters["edge.width"] <- input$edge_width
  plot_parameters["edge.color"] <- input$edge_color
  plot_parameters["edge.curved"] <- input$edge_curved
  
  # set node color for nodes selected in nodes data table
  plot_parameters[["vertex.color"]] <- ifelse(V(g)$id %in% selected_row_names, gbl_plot_sel_node_color, V(g)$color)
  plot_parameters[["vertex.frame.color"]] <- ifelse(V(g)$id %in% selected_row_names, "#000000", "gray")
  plot_parameters[["vertex.label.font"]] <- ifelse(V(g)$id %in% selected_row_names, 2, 1)
  
  # label.degree defines the position of the node labels, relative to the center of the nodes
  # it is interpreted as an angle in radian, zero means 'to the right', and 'pi' means to the left,
  # up is -pi/2 and down is pi/2. the default value is -pi/4.
  
  base_node_size <-input$igraph_node_base_size_slider
  base_label_size <- 0.8
  
  # label position - ADD TO UI
  label_dist <- input$node_label_dist
  label_degree <- input$node_label_rot
  
  # multiplier for normalized node sizes
  norm_multi <- 3
  igraph_vsize <- function(x) base_node_size + (((norm_values(x) + 0.1) * norm_multi) * node_size_multiplier)
  
  # node index mode 
  if (node_index_check) {
    plot_parameters[["vertex.label"]] <- sub("n", "", V(g)$id)
    plot_parameters[["vertex.label.color"]] <- "#000000"
    base_node_size <- 7
  }

  # set node size
  if (node_degree_type != "None") {
    node_size <- igraph_vsize(igraph::vertex_attr(g, node_degree_type))
  } else {
    node_size <- ((base_node_size + 0.1) * node_size_multiplier)
  }
  plot_parameters[["vertex.size"]] <- node_size

  # set unicode font for non windows - ADD TO UI
  if (.Platform$OS.type != "windows" &
      ("Arial Unicode MS" %in% VOSONDash::getSystemFontFamilies()) &
      input$macos_font_check) {
    plot_parameters["vertex.label.family"] <- plot_parameters["edge.label.family"] <- "Arial Unicode MS"
  } else {
    plot_parameters["vertex.label.family"] <- plot_parameters["edge.label.family"] <- "Arial"
  }
  
  # node labels when not in node index mode
  if (!node_index_check) {
    if (input$node_labels_check == FALSE) plot_parameters[["vertex.label"]] <- NA
    
    plot_parameters["vertex.label.cex"] <- base_label_size
    plot_parameters["vertex.label.dist"] <- label_dist
    plot_parameters["vertex.label.degree"] <- label_degree
  
    # node label attribute present
    labels <- ifelse(!is.null(igraph::vertex_attr(g, "label")), TRUE, FALSE)
  
    # if labels checked on
    if (input$node_labels_check == TRUE) {
      
      # if selected labels only
      if (input$node_sel_labels_check == TRUE) {
        label_values <- ifelse(
          labels,
          ifelse(V(g)$id %in% selected_row_names, ifelse(nchar(V(g)$label) > 0, V(g)$label, "-"), NA),
          ifelse(V(g)$id %in% selected_row_names, ifelse(nchar(V(g)$name) > 0, V(g)$name, "-"), NA)
        )
      # label all nodes
      } else {
        label_values <- ifelse(
          labels,
          ifelse(nchar(V(g)$label) > 0, V(g)$label, "-"),
          ifelse(nchar(V(g)$name) > 0, V(g)$name, "-")
        )
      }
      # set node labels
      plot_parameters[["vertex.label"]] <- label_values
      
      # set node label colors
      plot_parameters[["vertex.label.color"]] <- ifelse(
        V(g)$id %in% selected_row_names, gbl_sel_label_col,
        ifelse(is.null(V(g)$label.color), gbl_plot_def_label_color, V(g)$label.color)
      )
      
      # if node label size proportional to node size
      if (input$node_label_prop_size_check) {
        plot_parameters[["vertex.label.cex"]] <- switch(
          node_degree_type,
          "Degree" = (norm_values(V(g)$Degree)) + base_label_size,
          "Indegree" = (norm_values(V(g)$Indegree)) + base_label_size,
          "Outdegree" = (norm_values(V(g)$Outdegree)) + base_label_size,
          "Betweenness" = (norm_values(V(g)$Betweenness)) + base_label_size,
          "Closeness" = (norm_values(V(g)$Closeness)) + base_label_size,
          "None" = base_label_size
        )  
      } else {
        plot_parameters[["vertex.label.cex"]] <- input$node_label_size  # node label size - ADD TO UI
      }
    }
  }
  
  # set edge labels - ADD TO UI
  plot_parameters[["edge.label"]] <- NA
  if (input$edge_labels_check == TRUE & !is.null(input$edge_label_select)) {
    plot_parameters["edge.label.cex"] <- input$edge_label_size
    plot_parameters[["edge.label"]] <- edge_attr(g, input$edge_label_select)
  }
  
  # seed must be set before graph layout
  if (!is.null(graph_seed)) set.seed(graph_seed)
  
  # set graph layout
  graph_layout <- switch(
    chosen_layout,
    "Auto" = layout_nicely(g, dim = 2),
    "FR" = layout_with_fr(g, dim = 2, niter = input$graph_niter),
    "KK" = layout_with_kk(g, dim = 2),
    "DH" = layout_with_dh(g),
    "LGL" = layout_with_lgl(g),
    "DrL" = layout_with_drl(g),
    "GEM" = layout_with_gem(g),
    "MDS" = layout_with_mds(g),
    "Tree" = layout_as_tree(g, circular = TRUE),
    "Grid" = layout_on_grid(g),
    "Sphere" = layout_on_sphere(g),
    "Circle" = layout_in_circle(g),
    "Star" = layout_as_star(g),
    "Random" = layout_randomly(g),
    layout_nicely(g, dim = 2)
  )
  
  # if layout graphopt get additional options
  if (chosen_layout == "Graphopt") {
    graph_layout <- layout_with_graphopt(
      g,
      niter = input$graph_niter, 
      charge = input$graph_charge,
      mass = input$graph_mass,
      spring.length = input$graph_spr_len,
      spring.constant = input$graph_spr_const
    )
  }
  
  # graph spread option changes scale
  graph_layout <- igraph::norm_coords(graph_layout, ymin = -1, ymax = 1, xmin = -1, xmax = 1)
  plot_parameters["rescale"] <- FALSE
  plot_parameters[["layout"]] <- graph_layout * graph_spread
  
  # plot graph
  par(mar = rep(0, 4))
  do.call(plot.igraph, plot_parameters)
})
