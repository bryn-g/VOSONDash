# graph reactive variables
g_rv <- reactiveValues(
  data = NULL,
  dir = NULL,
  seed = NULL,
  is_igraph = FALSE,
  layout = NULL
)

# check input igraph data
r_is_data_valid <- reactive({
  data <- req(g_rv$data$data)
  ifelse((isTruthy(data) & ("igraph" %in% class(data))), TRUE, FALSE)
})

# event for graph data set by upload or collection
observeEvent(g_rv$data, {
  if (!r_is_data_valid()) {
    dash_logger("attempted to set invalid igraph data.")
    return()
  }
  
  removeCssClass(selector = "a[data-value = 'igraph']", class = "inactive_menu_link")
  removeCssClass(selector = "a[data-value = 'visNetwork']", class = "inactive_menu_link")
  removeCssClass(selector = "a[data-value = 'graph_info']", class = "inactive_menu_link")
  
  g_meta_rv$data <- g_rv$data$meta
  dash_logger(paste0("igraph data object loaded. ", g_meta_rv$data$name))
  
  g_nodes_rv$pruned <- c()
  dt_prev_sel$nodes <- c()
  
  reset_enable_g_ctrls()
  
  if (input$nav_sel_tab_id != "network_graphs_tab") {
    updateNavbarPage(session, "nav_sel_tab_id", selected = "network_graphs_tab")
  }
  
  if (!input$canvas_tab %in% c("igraph", "visNetwork")) {
    updateTabsetPanel(session, "canvas_tab", selected = "igraph")
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# set the base graph object from initial processing
r_graph_base <- reactive({
  req(r_is_data_valid())
  
  g <- f_set_id_and_label(g_rv$data$data)
  g <- tidygraph::as_tbl_graph(g)
  
  seed <- sample(1:20000, 1)
  updateNumericInput(session, "graph_seed_input", value = seed)
  g_rv$seed <- seed
  layout <- ifelse(isTruthy(isolate(input$graph_layout_select)), isolate(input$graph_layout_select), "FR")
  layout <- f_get_layout(g, seed, layout)
  g_layout_rv$coords_base <- f_get_coords(g, layout)
  
  # g_nodes_rv$cats <- VOSONDash::get_node_cats(g)
  g_nodes_rv$properties <- VOSONDash::get_attr_properties(g)
  
  g
})

observeEvent(g_layout_rv$coords_base, {
  g_layout_rv$coords <- g_layout_rv$coords_base
})

# modify graph id and labels
f_set_id_and_label <- function(g) {
  attr_v <- igraph::vertex_attr_names(g)
  attr_e <- igraph::edge_attr_names(g)
  
  # nodes
  if ("id" %in% attr_v) {
    if (!"id.imp" %in% attr_v) igraph::V(g)$id.imp <- igraph::V(g)$id
  }
  igraph::V(g)$id <- paste0("n", as.numeric(igraph::V(g))-1) # n0, n1 ..
  
  if (!igraph::is_named(g)) {
    igraph::V(g)$name <- igraph::V(g)$id
  } else {
    igraph::V(g)$id.n <- igraph::V(g)$id
    igraph::V(g)$id <- igraph::V(g)$name
  }
  
  if ("label" %in% attr_v) {
    if (!"label.imp" %in% attr_v) igraph::V(g)$label.imp <- igraph::V(g)$label
  }
  igraph::V(g)$label <- igraph::V(g)$name
  
  # edges
  if ("id" %in% attr_e) {
    if (!"id.imp" %in% attr_e) igraph::E(g)$id.imp <- igraph::E(g)$id
  }
  igraph::E(g)$id <- paste0("e", as.numeric(igraph::E(g))-1) # e0, e1 ..
  
  if ("label" %in% attr_e) {
    if (!"label.imp" %in% attr_e) igraph::E(g)$label.imp <- igraph::E(g)$label
  }
  igraph::E(g)$label <- igraph::E(g)$id
  
  g
}
