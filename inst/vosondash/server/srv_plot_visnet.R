r_graph_visnet_plot <- reactive({
  nodes <- r_graph_nodes_df()
  edges <- r_graph_edges_df()
  
  if (is.null(nodes) | is.null(edges)) return(NULL)
  if (nrow(nodes) < 1) return(NULL)
  
  isolate({
    # already dependencies of graphFilters, nodes, edges
    categorical_attributes <- graph_rv$graph_cats
    selected_categorical_attribute <- input$graph_cat_select
    gcs <- graph_rv$graph_cat_selected
  })
  
  nodes_rows_selected <- input$dt_nodes_rows_selected
  chosen_layout <- input$graph_layout_select
  graph_seed <- graph_rv$graph_seed
  node_degree_type <- input$graph_node_size_select
  node_size_multiplier <- input$graph_node_size_slider
  plot_height <- graph_rv$plot_height
  
  use_v_colors <- input$use_node_colors_check
  node_index_check <- input$node_index_check
  
  graph_layout <- switch(chosen_layout,
                         "Auto" = "layout_nicely",
                         "FR" = "layout_with_fr",   # Fruchterman-Reingold
                         "KK" = "layout_with_kk",   # Kamada-Kawai
                         "DH" = "layout_with_dh",   # Davidson-Harel
                         "LGL" = "layout_with_lgl", # Large Graph Layout
                         "Graphopt" = "layout_with_graphopt",
                         "DrL" = "layout_with_drl",
                         "GEM" = "layout_with_gem",
                         "MDS" = "layout_with_mds",
                         "Tree" = "layout_as_tree",
                         "Grid" = "layout_on_grid",
                         "Sphere" = "layout_on_sphere",
                         "Circle" = "layout_in_circle",
                         "Star" = "layout_as_star",
                         "Random" = "layout_randomly",
                         "layout_nicely")
  
  # nodes$font.size <- 24
  base_font_size <- 24
  
  # base_node_size <- 20
  base_node_size <- input$visgraph_node_base_size_slider
  norm_multi <- 5
  
  vis_vsize <- function(x) {
    base_node_size + (((norm_values(x) + 0.1) * norm_multi) * node_size_multiplier)
  }

  vis_lab_size <- function(x) {
    base_font_size + (((norm_values(x) + 0.1) * norm_multi) * node_size_multiplier)
  }
  
  node_size <- ((base_node_size + 0.1) * node_size_multiplier)
  lab_size <- base_font_size
  
  if (node_degree_type != "None") {
    v <- NULL
    if (node_degree_type %in% names(nodes)) {
      v <- nodes[[node_degree_type]]
    } else if (tolower(node_degree_type) %in% names(nodes)) {
      v <- nodes[[tolower(node_degree_type)]]
    }
    if (!is.null(v)) {
      node_size <- vis_vsize(v) 
      if (input$node_label_prop_size_check) {
        lab_size <- vis_lab_size(v)
      }
    }
  }
  
  nodes$size <- node_size
  nodes$font.size <- lab_size
  
  v_color_in_data <- FALSE
  if ("color" %in% names(nodes)) v_color_in_data <- TRUE
  
  if (nrow(nodes) > 0) {
    nodes$color.background <- as.character(gbl_plot_def_node_color)

    if (use_v_colors & v_color_in_data) { # added checkbox
      nodes$color.background <- nodes$color
    }
    
    nodes$font.color <- gbl_plot_def_label_color
    nodes$id <- nodes$name
  }
  
  # node colours (only if cat attr selected)
  if (length(categorical_attributes) > 0) { # only if have categorical attributes
    
    if (nchar(selected_categorical_attribute) && selected_categorical_attribute != "All") {
      
      categories <- categorical_attributes[[selected_categorical_attribute]]
      df <- data.frame('cat' = categories)
      if (nrow(df) > 0) {
        df$color <- gbl_plot_palette()[1:nrow(df)]
        if (use_v_colors == FALSE || !v_color_in_data) { # added checkbox
          nodes$color.background <- df$color[match(nodes[[selected_categorical_attribute]], df$cat)]
        }
      }
    }
  }
  
  sel_dt_row_names <- sel_subset <- c()
  if (length(nodes_rows_selected) > 0) {
    sel_dt_row_names <- row.names(nodes)[c(nodes_rows_selected)] # get df row names for nodes in dt selection
    sel_subset <- row.names(nodes) %in% sel_dt_row_names
    nodes$color.background[sel_subset] <- gbl_plot_sel_node_color
    nodes$font.color[sel_subset] <- gbl_sel_label_col
  }

  # node index
  if (node_index_check) {
    nodes$label <- sub("n", "", row.names(nodes))
    nodes$title <- nodes$id
    nodes$shape <- "circle"
  } else {
    nodes$shape <- "dot"
    nodes$title <- nodes$id
  }
  
  # node labels
  if (input$node_labels_check == TRUE) {
    nodes$title <- row.names(nodes)
    nodes <- dplyr::mutate(nodes, label = ifelse(is.na(.data$label), .data$id, .data$label))
    
    # selected only
    if (input$node_sel_labels_check == TRUE) {
      if (length(sel_subset)) {
        nodes$sel_label[sel_subset] <- nodes$label[sel_subset]
        nodes <- dplyr::mutate(nodes, label = ifelse(!is.na(.data$sel_label), .data$sel_label, ""),
                               sel_label = NULL) 
      }
    }
  } else {
    if (!node_index_check) nodes$label <- ""
  }
  
  if (!"width" %in% names(edges)) {
    if ("weight" %in% names(edges)) {
      edges$width <- edges$weight
    } else {
      medge <- input$graph_multi_edge_check # isolate(input$graph_multi_edge_check)
      if (medge == FALSE) {
        edges <- edges |>
          group_by(to, from) |>
          summarise(width = n(), .groups = "drop") |>
          ungroup()
      }
    }
  }
  
  category_selection <- NULL
  if (!is.null(gcs) && (!(gcs %in% c("All", "")))) {
    category_selection <- list(variable = gcs, multiple = TRUE)
  }
  
  if (!input$visnet_id_select_check) category_selection <- NULL
  
  if ("color" %in% names(nodes)) nodes <- dplyr::select(nodes, -color)

  use_mast_images <- FALSE
  # mast images
  if (input$mast_images) {
    if ("user.avatar" %in% colnames(nodes)) {
      use_mast_images <- TRUE
      img_shape <- "circularImage"
      if (input$mast_square_images) img_shape <- "image"
      nodes <- nodes |>
        dplyr::mutate(image = ifelse(is.na(.data$user.avatar),
                                     "www/mast.png",
                                     .data$user.avatar),
                      shape = img_shape)     
    }
  }
  
  vis_net <- visNetwork::visNetwork(nodes, edges, main = NULL)
  
  l_params <- list(vis_net, layout = graph_layout, randomSeed = graph_seed)
  if (chosen_layout %in% c("FR", "Graphopt")) l_params["niter"] <- input$graph_niter
  if (chosen_layout == "Graphopt") {
    l_params["charge"] = input$graph_charge
    l_params["mass"] = input$graph_mass
    l_params["spring.length"] = input$graph_spr_len
    l_params["spring.constant"] = input$graph_spr_const    
  }
  vis_net <- do.call(visIgraphLayout, l_params)
  
  vis_net <- vis_net |>
    visOptions(collapse = FALSE, 
               highlightNearest = list(enabled = TRUE, hover = TRUE),
               selectedBy = category_selection,
               nodesIdSelection = input$visnet_id_select_check,
               height = plot_height) |>
    visInteraction(multiselect = TRUE) |>
    visEvents(click = "function(v) { 
                // if (v.event.srcEvent.ctrlKey) {
                //   Shiny.onInputChange('vis_nbh_node_select', v.nodes);
                // } else {
                  Shiny.onInputChange('vis_node_select', v.nodes);
                // }
                }")
  
  if (input$mast_border_images) {
    if (use_mast_images) {
      vis_net <- vis_net |> visNodes(shapeProperties = list(useBorderWithImage = TRUE))
    } else {
      vis_net <- vis_net |> visNodes(shapeProperties = list(useBorderWithImage = FALSE))
    }
  }
  
  e_arrows <- e_smooth <- NULL
  if (graph_rv$graph_dir) { e_arrows <- "to" }
  if (isTruthy(input$graph_multi_edge_check) && input$graph_multi_edge_check == TRUE) { e_smooth <- list(enabled = TRUE, type = "diagonalCross") }
  
  vis_net <- vis_net |> visEdges(arrows = e_arrows,
                                  smooth = e_smooth,
                                  color = list(color = "#b0b0b0"))
  
  vis_net
})
