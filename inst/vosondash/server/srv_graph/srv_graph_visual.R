r_graph_visual <- reactive({
  g <- r_graph_filter()
  
  if (!isTruthy(g)) return(NULL)

  ## nodes ----
  
  g <- g |> tidygraph::as_tbl_graph() |> tidygraph::activate(nodes)
  node_attrs <- igraph::vertex_attr_names(g)
  
  # nodes selected
  sel_node_rows <- input$dt_nodes_rows_selected
  sel_node_row_names <- c()
  if (length(sel_node_rows)) {
    sel_node_row_names <- g |> dplyr::slice(sel_node_rows) |> pull(id)
  }
  g <- g |> dplyr::mutate(selected = ifelse(id %in% sel_node_row_names, TRUE, FALSE))
  
  # node size
  node_size_attr <- input$node_size_sel
  node_size_mplr <- input$node_size_slider
  
  # igraph node size
  base_node_size <-input$igraph_node_base_size_slider
  mpld_size <- (base_node_size + 0.1) * node_size_mplr
  norm_mplr <- 3
  igraph_node_size <- function(x) base_node_size + (((f_norm_vals(x) + 0.1) * norm_mplr) * node_size_mplr)
  
  if (node_size_attr != "None") {
    g <- g |> dplyr::mutate(size = igraph_node_size(.data[[node_size_attr]]))
  } else {
    g <- g |> dplyr::mutate(size = mpld_size)
  }
  
  # node label
  label_type <- g_nodes_rv$label_type
  label_selected <- g_nodes_rv$label_selected
  
  # igraph label size
  base_label_size <- 0.8
  
  g <- g |> dplyr::mutate(label = NA)
  
  if (isTruthy(label_type) & isTruthy(label_selected)) {
    
    if (label_type == "index") {
      g <- g |> dplyr::mutate(label = sub("n", "", id))
      
    } else if (label_type == "attribute") {
      if (label_selected %in% node_attrs) {
        g <- g |> dplyr::mutate(label = .data[[label_selected]])
      } else {
        g <- g |> dplyr::mutate(label = NA)
      }
    }
    
    if (input$node_sel_labels_chk) {
      g <- g |> dplyr::mutate(label = ifelse(selected == TRUE, label, NA))
    }
  } else {
    g <- g |> dplyr::mutate(label = NA)
  }
    
  # label.degree defines the position of the node labels, relative to the center of the nodes
  # it is interpreted as an angle in radian, zero means 'to the right', and 'pi' means to the left,
  # up is -pi/2 and down is pi/2. the default value is -pi/4.
    
  g <- g |> dplyr::mutate(
    label.font = ifelse(selected == TRUE, 2, 1),
    label.color = ifelse(selected == TRUE, gbl_sel_label_col, input$node_label_color),
    label.cex = input$node_label_size,
    label.dist = input$node_label_dist,
    label.degree = input$node_label_rot
  )
  
  base_label_size <- input$node_label_size
  g <- g |> dplyr::mutate(label.cex = base_label_size)
  
  if (input$node_label_prop_chk) {
    if (node_size_attr %in% node_attrs) {
      # label_sizes <- g |> dplyr::pull(.data[[node_size_attr]])
      # label_sizes <- (f_norm_vals(label_sizes)) + base_label_size
      
      g <- g |> dplyr::mutate(label.cex = f_norm_vals(.data[[node_size_attr]]) + base_label_size)
    }
  }

  # node color
  g <- g |> dplyr::mutate(frame.color = ifelse(selected == TRUE, "#000000", "gray"))
  
  
  if (!"color" %in% node_attrs) {
    g <- g |> dplyr::mutate(color = input$node_color)
  } else {
    if (!input$node_use_g_cols_chk) g <- g |> dplyr::mutate(color = input$node_color)
  }
  g <- g |> dplyr::mutate(color = ifelse(selected == TRUE, gbl_plot_sel_node_color, color))

  ## edges ----
  
  g <- g |> tidygraph::activate(edges) |>
    dplyr::mutate(color = input$edge_color,
                  width = input$edge_width,
                  curved = input$edge_curved,
                  arrow.size = input$edge_arrow_size,
                  arrow.width = input$edge_arrow_width)
  
  if (input$edge_labels_chk & !is.null(input$edge_label_sel)) {
    if (input$edge_label_sel != "None") {
      g <- g |>
        dplyr::mutate(label = .data[[input$edge_label_sel]],
                      label.cex = input$edge_label_size,
                      label.color = input$edge_label_color)
    }
  } else {
    g <- g |> dplyr::mutate(label = NA)
  }
  
  g
})