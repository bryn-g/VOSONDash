# graph nodes reactive variables
g_nodes_rv <- reactiveValues(
  attrs = NULL,
  
  labels = NULL,
  label_type = "none",
  label_selected = NULL,
  
  cats = NULL,
  cat_selected = "All",
  cat_sub_selected = "All",
  
  pruned = NULL,
  layout = NULL
)

# base graph node attribute list
r_node_attr_lst <- reactive({
  g <- req(r_graph_base())
  
  node_attr <- igraph::vertex_attr_names(g)
  init_attr_sel <- ifelse("imported_label" %in% node_attr, "imported_label", "id")
  node_attr <- sort(node_attr[!node_attr %in% c("label")])
  
  list(attrs = node_attr, sel = init_attr_sel)
})

# reset node size slider when changed to none
observeEvent(input$node_size_sel, {
  if (input$node_size_sel == "None") shinyjs::reset("node_size_slider")
}, ignoreInit = TRUE)
