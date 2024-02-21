r_graph_visnet_plot <- reactive({
  g <- r_graph_visual()
  
  if (is.null(g)) return(NULL)
  if (igraph::gorder(g) < 1) return(NULL)
  
  g <- g |> tidygraph::activate(nodes)
  
  node_attrs <- igraph::vertex_attr_names(g)
  edge_attrs <- igraph::edge_attr_names(g)
  
  n_edges <- igraph::gsize(g)
  
  # node color
  if ("color" %in% node_attrs) g <- g |> dplyr::mutate(color.background = color)
  
  # node size
  node_size_attr <- isolate(input$node_size_sel)
  node_size_mplr <- isolate(input$node_size_slider)
  
  base_node_size <- input$visnet_node_base_size_slider
  
  mpld_size <- (base_node_size + 0.1) * node_size_mplr
  norm_multi <- 5
  visnet_node_size <- function(x) base_node_size + (((f_norm_vals(x) + 0.1) * norm_multi) * node_size_mplr)
  
  if ("size" %in% node_attrs) {
    if (node_size_attr != "None") {
      g <- g |> dplyr::mutate(size = visnet_node_size(.data[[node_size_attr]]))
    } else {
      g <- g |> dplyr::mutate(size = mpld_size)
    }
  }
  
  # node labels
  label_type <- isolate(g_nodes_rv$label_type)
  
  g <- g |> dplyr::mutate(title = id)
  
  if (label_type == "index") {
    g <- g |> dplyr::mutate(shape = "circle")
  } else {
    g <- g |> dplyr::mutate(shape = "dot")
  }
  
  visnet_label_size <- function(x) base_font_size + (((f_norm_vals(x) + 0.1) * norm_multi) * node_size_mplr)
  
  base_font_size <- 24
  g <- g |> dplyr::mutate(font.size = base_font_size)

  if (isolate(input$node_label_prop_chk)) {
    if (node_size_attr %in% node_attrs) {
      g <- g |> dplyr::mutate(font.size = visnet_label_size(.data[[node_size_attr]]))
    }
  }
  
  if ("label.color" %in% node_attrs) g <- g |> dplyr::mutate(font.color = label.color)
  
  # mast images
  if (input$node_mtdn_img_chk) {
    if ("user.avatar" %in% node_attrs) {
      img_shape <- "circularImage"
      if (input$node_mtdn_img_sq_chk) img_shape <- "image"
      g <- g |>
        dplyr::mutate(image = user.avatar, shape = img_shape, brokenImage = "mast.png")
    }
  }
  
  g <- g |> tidygraph::activate(edges)
  g <- g |> dplyr::mutate(title = id)
  
  # if (igraph::gsize(g)) {
  #   if (!"width" %in% edge_attrs) {
  #     if ("weight" %in% edge_attrs) {
  #       g <- g |> dplyr::mutate(width = weight)
  #     } else {
  #       edge_multi_chk <- isolate(input$fltr_edge_multi_chk)
  #       if (!is.null(edge_multi_chk) && edge_multi_chk == FALSE) {
  #         g <- g |>
  #           dplyr::group_by(to, from) |>
  #           dplyr::summarise(width = n(), .groups = "drop") |>
  #           dplyr::ungroup()
  #       }
  #     }
  #   }
  # }

  # edges <- g |> tibble::as_tibble() |>
  #   dplyr::mutate(label = ifelse(is.na(label), "", label))
  #   
  # nodes <- g |> tidygraph::activate(nodes) |>
  #   tibble::as_tibble() |>
  #   dplyr::mutate(label = ifelse(is.na(label), "", label)) |>
  #   dplyr::relocate(id)
  
  # browser()
  edges <- g |> tidygraph::activate(edges) |>
    igraph::as_data_frame(what = c("edges")) |>
    dplyr::mutate(label = ifelse(is.na(label), "", label))
  
  nodes <- g |> tidygraph::activate(nodes) |>
    igraph::as_data_frame(what = c("vertices")) |>
    dplyr::mutate(label = ifelse(is.na(label), "", label))
  
  # browser()
  vis_net <- visNetwork::visNetwork(nodes, edges, main = NULL)
  
  coords <- g_layout_rv$coords
  coords$V2 <- (coords$V2 * -1) # V2 coords are mirrored with visnet
  
  l_params <- list(vis_net, layout = "layout.norm", randomSeed = isolate(g_rv$seed), layoutMatrix = as.matrix(coords))
  
  vis_net <- do.call(visIgraphLayout, l_params)
  
  plot_height <- g_plot_rv$height
  
  vis_net <- vis_net |>
    visNetwork::visOptions(collapse = FALSE, 
               highlightNearest = list(enabled = TRUE, hover = TRUE),
               # selectedBy = category_selection,
               nodesIdSelection = input$visnet_id_sel_chk,
               height = plot_height) |>
    visNetwork::visInteraction(multiselect = TRUE) |>
    visNetwork::visEvents(click = "function(v) { 
                // if (v.event.srcEvent.ctrlKey) {
                //   Shiny.onInputChange('vis_nbh_node_select', v.nodes);
                // } else {
                  Shiny.onInputChange('vis_node_select', v.nodes);
                // }
                }")
  
  if (input$node_mtdn_img_chk) {
    img_bord <- input$node_mtdn_img_bord_chk
    vis_net <- vis_net |> visNetwork::visNodes(shapeProperties = list(useBorderWithImage = isTruthy(img_bord)))
  }
  
  if (n_edges) {
    e_arrows <- NULL
    e_smooth <- NULL
    
    if (isTruthy(isolate(g_filter_rv$directed))) {
      if (input$visnet_edge_arrows_chk) e_arrows <- paste0(input$visnet_edge_arrows, collapse = ",")
    }
    
    if (input$visnet_edge_smooth_chk) {
      e_smooth <- list(enabled = TRUE, type = input$visnet_edge_smooth_type)
    }
    
    vis_net <- vis_net |> visNetwork::visEdges(arrows = e_arrows, smooth = e_smooth)
  }
  
  vis_net
})
