r_graph_igraph_plot <- reactive({
  if (!isTruthy(g_rv$is_igraph)) return(VOSONDash::get_empty_plot("No graph data."))
    
  g <- req(r_graph_filtered())
  
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
  # sel_edges <- input$dt_edges_rows_selected
  g_layout <- input$graph_layout_select
  g_seed <- g_rv$seed
  g_spread <- input$igraph_spread_slider  
  node_size_attr <- input$node_size_sel
  node_size_mplr <- input$node_size_slider  
  
  # use_node_index_label <- input$node_index_chk
  
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
    igraph::V(g)$color <- as.character(gbl_plot_def_node_color)  
  } else {
    if (!has_color_attr) {
      igraph::V(g)$color <- as.character(gbl_plot_def_node_color)
    }
  }
  
  # node colors if nodes have cat attrs and one is selected
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
  
  sel_node_row_names <- c()
  if (length(sel_node_rows) > 0) {
    sel_node_row_names <- row.names(graph_nodes_df)[c(sel_node_rows)]
  }
  
  # create a list for plot parameters
  igraph_params <- list(g)
  
  # edge attributes - ADD TO UI
  igraph_params["edge.arrow.size"] <- input$edge_arrow_size
  igraph_params["edge.arrow.width"] <- input$edge_arrow_width
  igraph_params["edge.width"] <- input$edge_width
  igraph_params["edge.color"] <- input$edge_color
  igraph_params["edge.curved"] <- input$edge_curved
  
  # set node color for nodes selected in nodes data table
  igraph_params[["vertex.color"]] <- ifelse(V(g)$id %in% sel_node_row_names, gbl_plot_sel_node_color, V(g)$color)
  igraph_params[["vertex.frame.color"]] <- ifelse(V(g)$id %in% sel_node_row_names, "#000000", "gray")
  igraph_params[["vertex.label.font"]] <- ifelse(V(g)$id %in% sel_node_row_names, 2, 1)
  
  # ----- node size
  base_node_size <-input$igraph_node_base_size_slider
  
  # multiplier for normalized node sizes
  norm_mplr <- 3
  igraph_node_size <- function(x) base_node_size + (((norm_values(x) + 0.1) * norm_mplr) * node_size_mplr)
  
  # set node size
  if (node_size_attr != "None") {
    node_size <- igraph_node_size(igraph::vertex_attr(g, node_size_attr))
  } else {
    node_size <- ((base_node_size + 0.1) * node_size_mplr)
  }
  igraph_params[["vertex.size"]] <- node_size
  
  # ----- node labels
  
  # label.degree defines the position of the node labels, relative to the center of the nodes
  # it is interpreted as an angle in radian, zero means 'to the right', and 'pi' means to the left,
  # up is -pi/2 and down is pi/2. the default value is -pi/4.
  
  label_type <- isolate(g_nodes_rv$label_type)
  
  if (label_type %in% c("index", "attribute")) {
    
    # set font family
    igraph_params["vertex.label.family"] <- "Arial"
    
    # set unicode font for non windows - ADD TO UI
    not_win <- .Platform$OS.type != "windows"
    arial_unicode <- "Arial Unicode MS" %in% VOSONDash::get_sysfont_names()
    
    if (not_win & arial_unicode & input$macos_font_check) {
      igraph_params["vertex.label.family"] <- "Arial Unicode MS"
    }
    
    base_label_size <- 0.8
    
    # attribute labels
    if (label_type == "attribute") {
      
      # if displaying labels for selected nodes only
      if (input$node_sel_labels_chk) {
        sel_nodes <- input$dt_nodes_rows_selected
        labels <- ifelse(V(g)$id %in% sel_nodes, V(g)$label, NA)
        
        # label all nodes
      } else {
        labels <- V(g)$label
      }
      
      # set node labels
      igraph_params[["vertex.label"]] <- labels
      igraph_params[["vertex.label.color"]]
      
      # set node label colors
      igraph_params[["vertex.label.color"]] <- ifelse(
        V(g)$id %in% sel_node_row_names, gbl_sel_label_col, 
        ifelse(is.null(V(g)$label.color), gbl_plot_def_label_color, V(g)$label.color)
      )
      
      # label position - ADD TO UI
      label_dist <- input$node_label_dist
      label_degree <- input$node_label_rot
      
      igraph_params["vertex.label.cex"] <- base_label_size
      igraph_params["vertex.label.dist"] <- label_dist
      igraph_params["vertex.label.degree"] <- label_degree
      
      # if node label size proportional to node size
      if (input$node_label_prop_chk) {
        igraph_params[["vertex.label.cex"]] <- switch(
          node_size_attr,
          "Degree" = (norm_values(V(g)$Degree)) + base_label_size,
          "Indegree" = (norm_values(V(g)$Indegree)) + base_label_size,
          "Outdegree" = (norm_values(V(g)$Outdegree)) + base_label_size,
          "Betweenness" = (norm_values(V(g)$Betweenness)) + base_label_size,
          "Closeness" = (norm_values(V(g)$Closeness)) + base_label_size,
          "None" = base_label_size
        )  
      } else {
        igraph_params[["vertex.label.cex"]] <- input$node_label_size  # node label size - ADD TO UI
      }
      
    } else {
      igraph_params[["vertex.label.color"]] <- "#000000"
      base_node_size <- 7
    }
  } else {
    igraph_params[["vertex.label"]] <- NA
  }
  
  # set edge labels - ADD TO UI
  igraph_params[["edge.label"]] <- NA
  if (input$edge_labels_chk == TRUE & !is.null(input$edge_label_sel)) {
    igraph_params["edge.label.cex"] <- input$edge_label_size
    igraph_params[["edge.label"]] <- edge_attr(g, input$edge_label_sel)
  }
  
  # seed must be set before graph layout
  if (!is.null(g_seed)) set.seed(g_seed)
  
  # set graph layout
  graph_layout <- switch(
    g_layout,
    "Auto" = igraph::layout_nicely(g, dim = 2),
    "FR" = igraph::layout_with_fr(g, dim = 2, niter = input$graph_niter),
    "KK" = igraph::layout_with_kk(g, dim = 2),
    "DH" = igraph::layout_with_dh(g),
    "LGL" = igraph::layout_with_lgl(g),
    "DrL" = igraph::layout_with_drl(g),
    "GEM" = igraph::layout_with_gem(g),
    "MDS" = igraph::layout_with_mds(g),
    "Tree" = igraph::layout_as_tree(g, circular = TRUE),
    "Grid" = igraph::layout_on_grid(g),
    "Sphere" = igraph::layout_on_sphere(g),
    "Circle" = igraph::layout_in_circle(g),
    "Star" = igraph::layout_as_star(g),
    "Random" = igraph::layout_randomly(g),
    igraph::layout_nicely(g, dim = 2)
  )
  
  # if layout graphopt get additional options
  if (g_layout == "Graphopt") {
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
  igraph_params["rescale"] <- FALSE
  igraph_params[["layout"]] <- graph_layout * g_spread
  
  #cat(file=stderr(), "running r_graph_igraph_plot\n")
  
  # plot graph
  par(mar = rep(0, 4))
  do.call(plot.igraph, igraph_params)
})
