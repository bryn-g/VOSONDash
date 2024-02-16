# modify base graph labels
f_set_id_and_label <- function(g) {
  
  # add node ids if not present
  attr_v <- igraph::vertex_attr_names(g)
  if (!("id" %in% attr_v)) igraph::V(g)$id <- paste0("n", as.numeric(igraph::V(g))-1) # n0, n1 ..
  if ("label" %in% attr_v) igraph::V(g)$imported_label <- igraph::V(g)$label
  
  # add edge ids if not present
  attr_e <- igraph::edge_attr_names(g)
  if (!("id" %in% attr_e)) igraph::E(g)$id <- paste0("e", as.numeric(igraph::E(g))-1) # n0, n1 ..
  if ("label" %in% attr_e) igraph::E(g)$imported_label <- igraph::E(g)$label
  
  g
}

# set labels on change of node attributes
observeEvent(g_nodes_rv$attrs, {
  req(g_nodes_rv$attrs)
  
  if (is.null(g_nodes_rv$label_selected)) {
    g_nodes_rv$label_selected <- ifelse("imported_label" %in% g_nodes_rv$attrs, "imported_label", "id")
  } else {
    if (!g_nodes_rv$label_selected %in% g_nodes_rv$attrs) g_nodes_rv$label_selected <- NULL
  }
  
  # sort and remove attribute option named label
  g_nodes_rv$labels <- c("", sort(g_nodes_rv$attrs[!"label" %in% g_nodes_rv$attrs]))
})

observeEvent(input$node_label_sel, {
  if (isTruthy(input$node_label_sel) & (input$node_label_sel != "None")) {
    g_nodes_rv$label_selected <- input$node_label_sel 
  }
}, ignoreInit = TRUE)

# update node attribute label select
observeEvent(g_nodes_rv$labels, {
  shinyjs::enable("node_label_sel")
  updateSelectInput(
    session,
    "node_label_sel",
    label = NULL,
    choices = r_node_attr_lst()$attrs,
    selected = r_node_attr_lst()$sel
  )
})

observeEvent(c(input$node_index_chk, input$node_labels_chk), {
  if (input$node_index_chk) {
    g_nodes_rv$label_type <- "index"
    updateCheckboxInput(session, "node_labels_chk", value = FALSE)
  } else if (input$node_labels_chk) {
    g_nodes_rv$label_type <- "attribute"
    updateCheckboxInput(session, "node_index_chk", value = FALSE)
  } else {
    g_nodes_rv$label_type <- "none"
  }
}, ignoreInit = TRUE)

# set labels on change of attributes
observeEvent(g_edges_rv$attrs, {
  req(g_edges_rv$attrs)
  
  if (is.null(g_edges_rv$label_selected)) {
    g_edges_rv$label_selected <- ifelse("imported_label" %in% g_edges_rv$attrs, "imported_label", "id")
  } else {
    if (!g_edges_rv$label_selected %in% g_edges_rv$attrs) g_edges_rv$label_selected <- NULL
  }
  
  # sort and remove attribute option named label
  g_edges_rv$labels <- c("", sort(g_edges_rv$attrs[!"label" %in% g_edges_rv$attrs]))
})

observeEvent(input$edge_label_sel, {
  if (isTruthy(input$edge_label_sel) & (input$edge_label_sel != "None")) {
    g_edges_rv$label_selected <- input$edge_label_sel 
  }
}, ignoreInit = TRUE)

# update attribute label select
observeEvent(g_edges_rv$labels, {
  shinyjs::enable("edge_label_sel")
  updatePickerInput(
    session,
    "edge_label_sel",
    label = NULL,
    choices = r_edge_attr_lst()$attrs,
    selected = r_edge_attr_lst()$sel
  )
})
