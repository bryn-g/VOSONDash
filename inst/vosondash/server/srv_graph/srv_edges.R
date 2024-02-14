# graph nodes reactive variables
g_edges_rv <- reactiveValues(
  attrs = NULL,
  
  labels = NULL,
  label_selected = NULL
)

# base graph node attribute list
r_edge_attr_lst <- reactive({
  g <- req(r_graph_base())
  
  edge_attr <- igraph::edge_attr_names(g)
  init_attr_sel <- ifelse("imported_label" %in% edge_attr, "imported_label", "id")
  edge_attr <- sort(edge_attr[!edge_attr %in% c("label")])
  
  list(attrs = edge_attr, sel = init_attr_sel)
})

observeEvent(input$reset_edges_btn, {
  if (isTruthy(input$reset_edges_btn)) {
    set_ctrl_state(
      c("edge_color",
      "edge_width",
      "edge_curved",
      "edge_arrow_size",
      "edge_arrow_width")
      , "reset")
  }
}, ignoreInit = TRUE)

# # base graph node attribute list
# r_edge_attr_lst <- reactive({
#   g <- req(r_graph_base())
#   
#   attr_e <- igraph::edge_attr_names(g)
#   
#   if ("label" %in% attr_e) {
#     igraph::E(ng_rv$graph_data)$imported_Label <- igraph::E(ng_rv$graph_data)$label
#     attr_e <- append(attr_e, "imported_Label")
#     sel <- "imported_Label"
#   }
#   label_list <- sort(attr_e[!attr_e %in% c("label")])
#   if (is.null(sel)) {
#     sel <- "None"
#   }
#   
#   edge_attr <- igraph::edge_attr_names(g)
#   edge_attr <- sort(edge_attr[!edge_attr %in% c("label")])
#   
#   list(attrs = node_attr
# })